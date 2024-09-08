//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Vector Processing Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boomvec.exu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.rocket
import freechips.rocketchip.tile

import boomvec.common._
import boomvec.util._


/**
 * Top level datapath that wraps the vector issue window, regfile, and arithmetic units.
 */
class VpPipeline(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  // issue parameters are:
  //  issueWidth      amount of available execution units
  //  numEntries      
  //  dispatchWidth   
  val VpIssueParams = issueParams.find(_.iqType == IQT_VP.litValue).get
  val vpWidth = VpIssueParams.issueWidth                      // 1
  val dispatchWidth = VpIssueParams.dispatchWidth             // 1 
  val numLlPorts = memWidth                                   // 1 
  val numWakeupPorts = VpIssueParams.issueWidth + numLlPorts  // 1 + 1 = 2
  // address size vector register?
  val vpPregSz = log2Ceil(numVecPhysRegs)                     // log2(32) = 5

  val io = IO(new Bundle {
    // branch update?
    val brupdate         = Input(new BrUpdateInfo())
    // flush the pipeline
    val flush_pipeline   = Input(Bool())
    // f control and status register
    //val fcsr_rm          = Input(UInt(width=freechips.rocketchip.tile.VPConstants.RM_SZ.W))
    // ???
    val status           = Input(new freechips.rocketchip.rocket.MStatus())
    // dispatched micro ops?
    val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // write ports?
    val ll_wports        = Flipped(Vec(memWidth, Decoupled(new ExeUnitResp(vLen)))) // from memory unit
    // ??? needed?
    //val from_int         = Flipped(Decoupled(new ExeUnitResp(vecWidth+1)))          // from integer RF
    // memory access?
    val to_sdq           = Decoupled(new ExeUnitResp(vLen))                           // to Load/Store Unit
    // ??? needed?
    //val to_int           = Decoupled(new ExeUnitResp(xLen))                         // to integer RF

    // wakeups ??
    val wakeups          = Vec(numWakeupPorts, Valid(new ExeUnitResp(vLen)))

    // writeback ??
    val wb_valids        = Input(Vec(numWakeupPorts, Bool()))
    val wb_pdsts         = Input(Vec(numWakeupPorts, UInt(width=vpPregSz.W)))

    // apparently debug stuff
    val debug_tsc_reg    = Input(UInt(width=xLen.W))
    val debug_wb_wdata   = Output(Vec(numWakeupPorts, UInt((vLen).W)))
  })

  
  //**********************************
  // construct all of the modules

  // vector execution unit
  // same as the FP pipeline there is only one execution unit
   val exe_units: Seq[VPExeUnit] = (0 until vpWidth) map { w =>
    Module(new VPExeUnit()).suggestName(s"vp_exe_unit_${w}")
  }

  // vector issue unit
  val issue_unit = Module(new IssueUnitCollapsing(
                          issueParams.find(_.iqType == IQT_VP.litValue).get,
                          numWakeupPorts))
  issue_unit.suggestName("vp_issue_unit")

  // vector register file
  // why *3? (prob. amount of read ports?)
  val numVrfReaders = 1
  val numVrfReadPorts = 3
  val numVrfWritePorts = 1

  val vregfile = Module(new RegisterFileSynthesizable(numVecPhysRegs,        // number of registers
                        numVrfReadPorts,                                     // number of read ports
                        numVrfWritePorts,                                    // number of write ports 
                        vLen,                                                // register width
                        Seq.fill(numVrfWritePorts){ false } ))               // No bypassing for any vector units 


  val vregister_read = Module(new RegisterRead(
                         issue_unit.issueWidth,                 // total issue width = 1
                         Seq(exe_units(0).supportedFuncUnits),  // sequence of SupportedFuncUnits
                         numVrfReadPorts,                       // number of read ports
                         Seq(3),                                // seq of number of read ports per exe unit
                         0, // No bypass                        // number of bypass ports of exe units
                         0,                                     // something bypass
                         vLen))                                 // register width

  // what does this do?
  // what is xLen?
  //val vp_bypasses = Wire(Vec(vpWidth, Valid(new ExeUnitResp(xLen+1))))
  //val vp_wakeups = Wire(Vec(numWakeupPorts, Valid(new Wakeup)))
  //io.wakeups := vp_wakeups

  //*************************************************************
  // Issue window logic

  val iss_valids = Wire(Vec(numVrfReaders, Bool()))
  val iss_uops   = Wire(Vec(numVrfReaders, new MicroOp()))

  // connect stuff to the issue unit
  // ???
  issue_unit.io.tsc_reg := io.debug_tsc_reg
  issue_unit.io.brupdate := io.brupdate
  issue_unit.io.flush_pipeline := io.flush_pipeline
  // Don't support ld-hit speculation to FP window.
  for (w <- 0 until memWidth) {
    issue_unit.io.spec_ld_wakeup(w).valid := false.B
    issue_unit.io.spec_ld_wakeup(w).bits := 0.U
  }
  issue_unit.io.ld_miss := false.B


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
    iss_valids(i) := issue_unit.io.iss_valids(i)
    iss_uops(i) := issue_unit.io.iss_uops(i)

    var fu_types = exe_units(i).io.fu_types
    issue_unit.io.fu_types(i) := fu_types
  }

  // Wakeup
  for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_ports) {
    issue_wakeup.valid := writeback.valid
    issue_wakeup.bits.pdst  := writeback.bits.uop.pdst
    issue_wakeup.bits.poisoned := false.B
  }

  issue_unit.io.pred_wakeup_port.valid := false.B
  issue_unit.io.pred_wakeup_port.bits := DontCare


  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------

  // Register Read <- Issue (rrd <- iss)
  vregister_read.io.rf_read_ports <> vregfile.io.read_ports
  vregister_read.io.prf_read_ports map { port => port.data := false.B }

  vregister_read.io.iss_valids <> iss_valids
  vregister_read.io.iss_uops := iss_uops

  vregister_read.io.brupdate := io.brupdate
  vregister_read.io.kill := io.flush_pipeline


  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------

  exe_units.map(_.io.brupdate := io.brupdate)

  for (w <- 0 until vpWidth) {
    exe_units(w).io.req <> vregister_read.io.exe_reqs(w)
  }

  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------

  // no arbiter needed? 
  // do something with io.ll_wports(0)
  // this is probably wrong :D
  var w_cnt = 1
  for (i <- 1 until memWidth) {
    vregfile.io.write_ports(w_cnt) := RegNext(WritePort(io.ll_wports(i), vpPregSz, vLen, RT_FIX))
    vregfile.io.write_ports(w_cnt).bits.data := RegNext(io.ll_wports(i).bits.data)
    w_cnt += 1
  }
  for (eu <- exe_units) {
    vregfile.io.write_ports(w_cnt).valid     := eu.io.vresp.valid && eu.io.vresp.bits.uop.rf_wen
    vregfile.io.write_ports(w_cnt).bits.addr := eu.io.vresp.bits.uop.pdst
    vregfile.io.write_ports(w_cnt).bits.data := eu.io.vresp.bits.data
    eu.io.vresp.ready                        := true.B
    // some assertions missing here, not important
    w_cnt += 1
  }

  require (w_cnt == vregfile.io.write_ports.length)


  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------

  io.wakeups(0).valid := vregfile.io.write_ports(0).valid
  io.wakeups(0).bits := vregfile.io.write_ports(0).bits

  w_cnt = 1
  for (i <- 1 until memWidth) {
    io.wakeups(w_cnt) := io.ll_wports(i)
    io.wakeups(w_cnt).bits.data := io.ll_wports(i).bits.data
    w_cnt += 1
  }
  for (eu <- exe_units) {
      val exe_resp = eu.io.vresp
      val wb_uop = eu.io.vresp.bits.uop
      val wport = io.wakeups(w_cnt)
      wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FIX
      wport.bits := exe_resp.bits

      w_cnt += 1

      assert(!(exe_resp.valid && wb_uop.uses_ldq))
      assert(!(exe_resp.valid && wb_uop.uses_stq))
      assert(!(exe_resp.valid && wb_uop.is_amo))
  }

  for ((wdata, wakeup) <- io.debug_wb_wdata zip io.wakeups) {
    wdata := wakeup.bits.data
  }
  exe_units.map(_.io.status := io.status)


  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------

  for (w <- 0 until exe_units.length) {
    exe_units(w).io.req.bits.kill := io.flush_pipeline
  }

  
  override def toString: String =
    (BoomCoreStringPrefix("===VP Pipeline===") + "\n"
    + exe_units.map(_.toString).mkString("\n") + "\n"
    + vregfile.toString
    + BoomCoreStringPrefix(
      "Num Wakeup Ports      : " + numWakeupPorts))
}