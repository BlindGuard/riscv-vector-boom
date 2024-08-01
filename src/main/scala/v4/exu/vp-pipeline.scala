//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Vector Processing Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v4.exu

import chisel3._
import chisel3.util._

import boom.v4.common._
import boom.v4.util._


/**
 * Top level datapath that wraps the vector issue window, regfile, and arithmetic units.
 */
class VpPipeline(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  // issue parameters are:
  //  issueWidth      amount of available execution units
  //  numEntries      
  //  dispatchWidth   
  val VpIssueParams = issueParams.find(_.iqType == IQ_VP).get
  val dispatchWidth = VpIssueParams.dispatchWidth
  val numLlPorts = lsuWidth                           
  val numWakeupPorts = VpIssueParams.issueWidth + numLlPorts
  // physical register size?
  val vpPregSz = log2Ceil(numVpPhysRegs)

  val io = IO(new Bundle {
    // branch update?
    val brupdate         = Input(new BrUpdateInfo())
    // flush the pipeline
    val flush_pipeline   = Input(Bool())
    // ???
    val fcsr_rm          = Input(UInt(width=freechips.rocketchip.tile.VPConstants.RM_SZ.W))
    // ???
    val status           = Input(new freechips.rocketchip.rocket.MStatus())
    // dispatched micro ops?
    val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // +1 for recoding.
    // write ports?
    val ll_wports        = Flipped(Vec(lsuWidth, Valid(new ExeUnitResp(vecWidth+1)))) // from memory unit
    // ??? needed?
    val from_int         = Flipped(Decoupled(new ExeUnitResp(vecWidth+1)))            // from integer RF
    // memory access?
    val dgen             = Valid(new MemGen)                                      // to Load/Store Unit
    // ??? needed?
    val to_int           = Decoupled(new ExeUnitResp(xLen))                       // to integer RF

    // wakeups ??
    val wakeups          = Vec(numWakeupPorts, Valid(new Wakeup))
    // writeback
    val wb               = Vec(numWakeupPorts, Valid(new ExeUnitResp(vLen+1)))

    val debug_tsc_reg    = Input(UInt(width=xLen.W))
  })

  
  //**********************************
  // construct all of the modules

  // vector execution unit
  // same as the FP pipeline there is only one execution unit
   val exe_units: Seq[VPExeUnit] = (0 until vpWidth) map { w =>
    Module(new VPExeUnit()).suggestName(s"vp_exe_unit_${w}")
  }

  // vector issue unit
  val issue_unit     = IssueUnit(VpIssueParams, numWakeupPorts, false, false)
  issue_unit.suggestName("vp_issue_unit")

  // vector register file
  // why *3? (prob. amount of read ports?)
  val numVrfLogicalReadPorts = vpWidth * 3
  val numVrfWritePorts = vpWidth + lsuWidth
  val VregfileBankedWriteArray = Seq.fill(numFrfWritePorts) { None }

  // difference logical/physical read port
  // banked write port array, for what??
  val vregfile       = Module(new BankedRF(
    UInt((vecWidth).W),           // width of data (?)
    numVrfBanks,                  // number of register banks
    numVrfLogicalReadPorts,       // number of logical read ports per bank
    numVcPhysRegs,                // number of registers (total?)
    numVrfLogicalReadPorts,       // total number of logical read ports
    numVrfReadPorts,              // number of physical read ports
    numVrfWritePorts,             // number of write ports 
    VregfileBankedWriteArray,     // banked write port array ???
    "Vector"                      // type description
  ))

  // what does this do?
  // what is xLen?
  val vp_bypasses = Wire(Vec(vpWidth, Valid(new ExeUnitResp(xLen+1))))
  val vp_wakeups = Wire(Vec(numWakeupPorts, Valid(new Wakeup)))
  io.wakeups := vp_wakeups

  //*************************************************************
  // Issue window logic

  // ???
  val iss_uops   = issue_unit.io.iss_uops

  // connect stuff to the issue unit
  // ???
  issue_unit.io.tsc_reg := io.debug_tsc_reg
  issue_unit.io.brupdate := io.brupdate
  issue_unit.io.flush_pipeline := io.flush_pipeline
  issue_unit.io.squash_grant := exe_units.map(_.io_squash_iss).reduce(_||_)


  //-------------------------------------------------------------
  // **** Dispatch Stage ****
  //-------------------------------------------------------------
  // looks like this can just be copied from fp
  
  // Input (Dispatch)
  for (w <- 0 until dispatchWidth) {
    issue_unit.io.dis_uops(w) <> io.dis_uops(w)
  }


  //-------------------------------------------------------------
  // **** Issue Stage ****
  //-------------------------------------------------------------
  // completely copied from fp 

  // Output (Issue)
  for (i <- 0 until issue_unit.issueWidth) {

    var fu_types = exe_units(i).io_ready_fu_types
    issue_unit.io.fu_types(i) := fu_types

  }

  // Wakeup
  issue_unit.io.wakeup_ports := vp_wakeups

  issue_unit.io.pred_wakeup_port.valid := false.B
  issue_unit.io.pred_wakeup_port.bits := DontCare
  issue_unit.io.child_rebusys := 0.U

  issue_unit.io.iss_uops zip exe_units map { case (i, u) => u.io_iss_uop := i }


  //-------------------------------------------------------------
  // **** Register Arbitrate Stage ****
  //-------------------------------------------------------------
  // completely copied from fp

  var rd_idx = 0
  for (unit <- exe_units) {
    for (i <- 0 until 3) {
      vregfile.io.arb_read_reqs(rd_idx) <> unit.io_arb_frf_reqs(i)
      rd_idx += 1
    }
  }
  require(rd_idx == numFrfLogicalReadPorts)


  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------

  var rd_idx = 0
  for (unit <- exe_units) {
    for (i <- 0 until 3) {
      vregfile.io.arb_read_reqs(rd_idx) <> unit.io_arb_frf_reqs(i)
      rd_idx += 1
    }
  }
  require(rd_idx == numFrfLogicalReadPorts)

  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------

  exe_units.map(_.io_brupdate := io.brupdate)


  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------

  // writeback arbiter
  // arbiter - controls access to shared resource between n producers
  val ll_wbarb = Module(new Arbiter(new ExeUnitResp(vecWidth+1), 1 + // mem
  ))

  // Hookup load writeback
  // nothing type specific here
  ll_wbarb.io.in(0).valid := RegNext(io.ll_wports(0).valid &&
    !IsKilledByBranch(io.brupdate, io.flush_pipeline, io.ll_wports(0).bits))
  ll_wbarb.io.in(0).bits  := RegNext(UpdateBrMask(io.brupdate, io.ll_wports(0).bits))
  ll_wbarb.io.in(0).bits.data := recode(RegNext(io.ll_wports(0).bits.data),
                                        RegNext(io.ll_wports(0).bits.uop.mem_size =/= 2.U))

  // Cut up critical path by delaying the write by a cycle.
  // Wakeup signal is sent on cycle S0, write is now delayed until end of S1,
  // but Issue happens on S1 and RegRead doesn't happen until S2 so we're safe.
  fregfile.io.write_ports(0).valid := ll_wbarb.io.out.valid && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FLT
  fregfile.io.write_ports(0).bits.addr := ll_wbarb.io.out.bits.uop.pdst
  fregfile.io.write_ports(0).bits.data := ll_wbarb.io.out.bits.data

  assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.

  var w_cnt = 1
  for (i <- 1 until lsuWidth) {
    vregfile.io.write_ports(w_cnt).valid := RegNext(io.ll_wports(i).valid)
    vregfile.io.write_ports(w_cnt).bits.addr := RegNext(io.ll_wports(i).bits.uop.pdst)
    vregfile.io.write_ports(w_cnt).bits.data := recode(RegNext(io.ll_wports(i).bits.data),
                                                       RegNext(io.ll_wports(i).bits.uop.mem_size =/= 2.U))
    w_cnt += 1
  }
  for (eu <- exe_units) {
    vregfile.io.write_ports(w_cnt).valid     := eu.io_vpu_resp.valid && eu.io_vpu_resp.bits.uop.dst_rtype === RT_FLT
    vregfile.io.write_ports(w_cnt).bits.addr := eu.io_vpu_resp.bits.uop.pdst
    vregfile.io.write_ports(w_cnt).bits.data := eu.io_vpu_resp.bits.data
    w_cnt += 1
  }
  for (w <- 0 until vpWidth) {
    fp_bypasses(w).valid := exe_units(w).io_vpu_resp.valid && exe_units(w).io_vpu_resp.bits.uop.dst_rtype === RT_FLT
    fp_bypasses(w).bits  := exe_units(w).io_vpu_resp.bits
  }

  require (w_cnt == vregfile.io.write_ports.length)


  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------

  var idx = 0
  for (w <- 0 until vpWidth) {
    vp_wakeups(idx)  = exe_units(w).io_wakeup
    io.wb(idx) := exe_units(w).io_vpu_resp
    idx += 1
  }

  vp_wakeups(idx).valid    := ll_wbarb.io.out.valid
  vp_wakeups(idx).bits.uop := ll_wbarb.io.out.bits.uop
  vp_wakeups(idx).bits.speculative_mask := 0.U
  vp_wakeups(idx).bits.bypassable := false.B
  vp_wakeups(idx).bits.rebusy := false.B

  io.wb(idx) := ll_wbarb.io.out
  ll_wbarb.io.out.ready := true.B
  idx += 1

  for (i <- 1 until lsuWidth) {
    val wb = RegNext(UpdateBrMask(io.brupdate, io.flush_pipeline, io.ll_wports(i)))
    vp_wakeups(idx).valid := wb.valid && wb.bits.uop.dst_rtype === RT_FLT
    vp_wakeups(idx).bits.uop := wb.bits.uop
    vp_wakeups(idx).bits.speculative_mask := 0.U
    vp_wakeups(idx).bits.bypassable := false.B
    vp_wakeups(idx).bits.rebusy := false.B


    io.wb(idx) := wb
    io.wb(idx).bits.data := recode(
      RegNext(io.ll_wports(i).bits.data),
      RegNext(io.ll_wports(i).bits.uop.mem_size =/= 2.U)
    )
    idx += 1
  }
  require (idx == numWakeupPorts)
  exe_units.map(_.io_fcsr_rm := io.fcsr_rm)
  exe_units.map(_.io_status := io.status)


  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------

  for (w <- 0 until exe_units.length) {
    exe_units(w).io_kill := io.flush_pipeline
  }

  
  override def toString: String =
    (BoomCoreStringPrefix("===VP Pipeline===") + "\n"
    + exe_units.map(_.toString).mkString("\n") + "\n"
    + vregfile.toString
    + BoomCoreStringPrefix(
      "Num Wakeup Ports      : " + numWakeupPorts))
}