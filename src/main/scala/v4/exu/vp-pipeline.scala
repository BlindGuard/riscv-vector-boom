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
 * Top level datapath that wraps the floating point issue window, regfile, and arithmetic units.
 */
class VpPipeline(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val VpIssueParams = issueParams.find(_.iqType == IQ_FP).get
  val dispatchWidth = VpIssueParams.dispatchWidth
  val numLlPorts = lsuWidth                           
  val numWakeupPorts = VpIssueParams.issueWidth + numLlPorts
  // physical register size?
  val fpPregSz = log2Ceil(numFpPhysRegs)

  val io = IO(new Bundle {
    // branch update?
    val brupdate         = Input(new BrUpdateInfo())
    // flush the pipeline
    val flush_pipeline   = Input(Bool())
    // ???
    val fcsr_rm          = Input(UInt(width=freechips.rocketchip.tile.FPConstants.RM_SZ.W))
    // ???
    val status           = Input(new freechips.rocketchip.rocket.MStatus())
    // dispatched micro ops?
    val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

    // +1 for recoding.
    // write ports?
    val ll_wports        = Flipped(Vec(lsuWidth, Valid(new ExeUnitResp(fLen+1)))) // from memory unit
    // ??? needed?
    val from_int         = Flipped(Decoupled(new ExeUnitResp(fLen+1)))            // from integer RF
    // memory access?
    val dgen             = Valid(new MemGen)                                      // to Load/Store Unit
    // ??? needed?
    val to_int           = Decoupled(new ExeUnitResp(xLen))                       // to integer RF

    // wakeups ??
    val wakeups          = Vec(numWakeupPorts, Valid(new Wakeup))
    // writeback
    val wb               = Vec(numWakeupPorts, Valid(new ExeUnitResp(fLen+1)))

    val debug_tsc_reg    = Input(UInt(width=xLen.W))
  })

  
  //**********************************
  // construct all of the modules

  // vector execution unit

  // vector issue unit

  // vector register file


  //*************************************************************
  // Issue window logic

  // ????


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
  issue_unit.io.wakeup_ports := fp_wakeups

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
      fregfile.io.arb_read_reqs(rd_idx) <> unit.io_arb_frf_reqs(i)
      rd_idx += 1
    }
  }
  require(rd_idx == numFrfLogicalReadPorts)


  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------


  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------


  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------


  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------


  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------
}