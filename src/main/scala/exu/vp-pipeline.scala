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
  * Extends the width contained in an ExeUnitResp.
  *
  * @param dataWidthIn starting width
  * @param dataWidthOut desired width
  */
class DataWidthConverter(
  dataWidthIn: Int,
  dataWidthOut: Int,
)(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ExeUnitResp(dataWidthIn)))
    val out = Decoupled(new ExeUnitResp(dataWidthOut))
  })
  io.in.ready := true.B
  io.out.valid := false.B

  io.out.bits.fflags      := io.in.bits.fflags      // is this floating point flags?
  io.out.bits.predicated  := io.in.bits.predicated

  var extendBy = dataWidthIn / dataWidthOut
  when(io.out.ready && io.in.valid)
  {
    io.out.bits.data  := Wire(Cat(Fill(extendBy, io.in.bits.data)))
    io.out.valid := true.B
  }
}


/**
 * Top level datapath that wraps the vector issue window, regfile, and arithmetic units.
 */
class VpPipeline(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  // issue parameters are:
  //  issueWidth      amount of available execution units
  //  numEntries      number of entries in issue queue
  //  dispatchWidth   number of execution units
  val VpIssueParams = issueParams.find(_.iqType == IQT_VP.litValue).get
  val vpWidth = VpIssueParams.issueWidth                      // 1
  val dispatchWidth = VpIssueParams.dispatchWidth             // 1 
  val numLlPorts = memWidth                                   // 1 
  val numWakeupPorts = VpIssueParams.issueWidth + numLlPorts  // 1 + 1 = 2
  // address size vector register
  val vpPregSz = log2Ceil(numVecPhysRegs)                     // log2(32) = 5

  val io = IO(new Bundle {
    // dispatched micro ops
    val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // ---- Branch Control and Status ----
    // branch update, includes info on miss predictions
    val brupdate         = Input(new BrUpdateInfo())
    // when true, flush the pipeline
    val flush_pipeline   = Input(Bool())
    // status registers
    val status           = Input(new freechips.rocketchip.rocket.MStatus())


    // ---- Memory Access ----
    // write ports?
    val ll_wports        = Flipped(Vec(memWidth, Decoupled(new ExeUnitResp(vLen))))   // from memory unit
    // needed to read scalar values, eg base memory address
    val from_int         = Flipped(Decoupled(new ExeUnitResp(xLen)))                  // from integer RF
    // access to the store queue, can only handle scalar values
    // as this uses the scalar memory unit
    val to_sdq           = Decoupled(new ExeUnitResp(vLen))                           // to Load/Store Unit
    // direct memory access for the VPU
    val mem              = new rocket.HellaCacheIO()

    // ---- Wakeups and Writeback ----
    val wakeups          = Vec(numWakeupPorts, Valid(new ExeUnitResp(vLen)))

    // writeback valid and address 
    val wb_valids        = Input(Vec(numWakeupPorts, Bool()))
    val wb_pdsts         = Input(Vec(numWakeupPorts, UInt(width=vpPregSz.W)))

    // apparently debug stuff?
    val debug_tsc_reg    = Input(UInt(width=xLen.W))
    val debug_wb_wdata   = Output(Vec(numWakeupPorts, UInt((vLen).W)))
  })

  
  //**********************************
  // construct all of the modules

  // vector execution unit
  // for now this contains only one execution unit
   val exe_units: Seq[VPExeUnit] = (0 until vpWidth) map { w =>
    Module(new VPExeUnit()).suggestName(s"vp_exe_unit_${w}")
  }
  exe_units(0).v_io.mem <> io.mem

  // vector issue unit
  // collects dispatched MicroOp and issues them 
  // when they are ready
  val issue_unit = Module(new IssueUnitCollapsing(
                          issueParams.find(_.iqType == IQT_VP.litValue).get,
                          numWakeupPorts))
  issue_unit.suggestName("vp_issue_unit")

  // vector register file
  val numVrfReaders = 1       // only one register read unit that reads from the register file
  val numVrfReadPorts = 3     // three read ports?, should probably only be two, 
                              // as vector inst have only 2 register arguments
  val numVrfWritePorts = 2    // two write ports, one from memory unit, one from VPU execution unit
  val vregfile = Module(new RegisterFileSynthesizable(numVecPhysRegs,        // number of registers
                        numVrfReadPorts,                                     // number of read ports
                        numVrfWritePorts,                                    // number of write ports 
                        vLen,                                                // register width
                        Seq.fill(numVrfWritePorts){ false } ))               // No bypassing for any vector units 

  // component that reads data from the register file
  // before handing the MicroOp to the execution unit
  val vregister_read = Module(new RegisterRead(
                         issue_unit.issueWidth,                 // total issue width = 1
                         Seq(exe_units(0).supportedFuncUnits),  // sequence of SupportedFuncUnits
                         numVrfReadPorts,                       // number of read ports
                         Seq(3),                                // seq of number of read ports per exe unit
                         0, // No bypass                        // number of bypass ports of exe units
                         0,                                     // something with bypass, not needed
                         vLen))                                 // register width

  // converter that widens loads from the integer register file to the vector width
  // this is a small hack so the memory unit does not have to be changed
  val from_mem_conv = Module(new DataWidthConverter(xLen, vLen))

  //*************************************************************
  // Issue window logic
  
  // wires for issue window output
  val iss_valids = Wire(Vec(numVrfReaders, Bool()))
  val iss_uops   = Wire(Vec(numVrfReaders, new MicroOp()))

  // connect stuff to the issue unit
  // connect branch controls and debug signals
  // from outside the pipeline to issue unit
  issue_unit.io.tsc_reg := io.debug_tsc_reg
  issue_unit.io.brupdate := io.brupdate
  issue_unit.io.flush_pipeline := io.flush_pipeline
  // Not sure yet what this does, Comment from floating point pipeline is:
  // "Don't support ld-hit speculation to FP window."
  for (w <- 0 until memWidth) {
    issue_unit.io.spec_ld_wakeup(w).valid := false.B
    issue_unit.io.spec_ld_wakeup(w).bits := 0.U
  }
  issue_unit.io.ld_miss := false.B


  //-------------------------------------------------------------
  // **** Dispatch Stage ****
  //-------------------------------------------------------------
  
  // input from dispatcher
  // connect dispatched microop into the issue unit
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

    // connect functional unit type of execution unit
    // to the issue unit
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
  // connect branch update to execution unit
  exe_units.map(_.io.brupdate := io.brupdate)

  // connect the FuncUnitReq from the register read unit
  // to the execution unit
  for (w <- 0 until vpWidth) {
    exe_units(w).io.req <> vregister_read.io.exe_reqs(w)
  }

  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------

  var w_cnt = 0
  // connect the response from the memory unit into the register file
  // for now, this response goes through a converted to pad it out to
  // the complete vector length
  for (i <- 0 until memWidth) {
    vregfile.io.write_ports(w_cnt) := RegNext(WritePort(from_mem_conv.io.out, vpPregSz, vLen, RT_FIX))
    vregfile.io.write_ports(w_cnt).bits.data := RegNext(from_mem_conv.io.out.bits.data)
    w_cnt += 1
  }
  // connect the response from the execution unit to the register file
  for (eu <- exe_units) {
    vregfile.io.write_ports(w_cnt).valid     := eu.io.vresp.valid && eu.io.vresp.bits.uop.rf_wen
    vregfile.io.write_ports(w_cnt).bits.addr := eu.io.vresp.bits.uop.pdst
    vregfile.io.write_ports(w_cnt).bits.data := eu.io.vresp.bits.data
    eu.io.vresp.ready                        := true.B
    // some assertions missing here, not important
    w_cnt += 1
  }

  // register file should have 2 write ports
  require (w_cnt == vregfile.io.write_ports.length)


  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------

  from_mem_conv.io.in <> io.from_int//io.ll_wports(0)

  io.wakeups(0).valid := from_mem_conv.io.out.valid 
  io.wakeups(0).bits := from_mem_conv.io.out.bits

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