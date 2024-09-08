package boomvec.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters

import freechips.rocketchip.rocket
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}

import boomvec.common._

/** Find out what these do:
  * 
  * HasFPUParameters
  * FPUCtrlSigs
  * 
  * 
  * 
  */

// copied signals from HasFPUCtrlSigs
// -> find out what these all do
// and what is needed for vpu
trait HasVPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
}

// what is dfmaLatency??
case class VPUParams(
  vLen: Int = 512,
  dfmaLatency: Int = 4,
  vectorLanes: Int = 8
)

trait HasVPUParameters {
  // HasFPUParameters here:
  // https://github.com/chipsalliance/rocket-chip/blob/dbcb06afe1c76d1129cb6d264949322a34c37185/src/main/scala/tile/FPU.scala#L304
  //val vLen: Int
}

// ????
object VPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}

class VPUCtrlSigs extends Bundle with HasVPUCtrlSigs

/**
 * FP Decoder for the VPU
 */
class UOPCodeVPUDecoder(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val uopc = Input(Bits(UOPC_SZ.W))
    val sigs = Output(new VPUCtrlSigs())
  })

  // values for control signals?
  // probably no, yes, don't care
  val X = BitPat("b?")
  val Y = BitPat("b1")
  val N = BitPat("b0")
  
  val default: List[BitPat] = List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

  // looks like this is a table to map uOPs to control signals?
  val instruction_table: Array[(BitPat, List[BitPat])] =
    // copied signals from FPUCtrlSigs
    //                                       swap12         fma
    //                                       | swap32       | div
    //                                       | | typeTagIn  | | sqrt
    //                            ldst       | | | typeTagOut | | wflags
    //                            | wen      | | | | from_int | | |
    //                            | | ren1   | | | | | to_int | | |
    //                            | | | ren2 | | | | | | fastpipe |
    //                            | | | | ren3 | | | | | |  | | | |
    //                            | | | | |  | | | | | | |  | | | |
    Array(
      BitPat(uopFNMSUB_S) -> List(X,X,Y,Y,Y, N,N,X,X,N,N,N, Y,N,N,Y)
    )

  val decoder = rocket.DecodeLogic(io.uopc, default, instruction_table)
}

/**
 * Bundle representing data to be sent to the VPU
 */
class VpuReq()(implicit p: Parameters) extends BoomBundle
{
  val uop      = new MicroOp()
  val rs1_data = Bits(512.W)
  val rs2_data = Bits(512.W)
}

// class VecInput(implicit p: Parameters) extends CoreBundle()(p) with HasVPUCtrlSigs {
//   val rm = Bits(VPConstants.RM_SZ.W)
//   val fmaCmd = Bits(2.W)
//   val typ = Bits(2.W)
//   val fmt = Bits(2.W)
//   val in1 = Bits((fLen+1).W)
//   val in2 = Bits((fLen+1).W)
//   val in3 = Bits((fLen+1).W)

// }

class VectorLane(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val in1 = Input(UInt(65.W))
    val in2 = Input(UInt(65.W))
    val fn = Input(UInt((new freechips.rocketchip.rocket.ALUFN).SZ_ALU_FN.W))

    val out = Bits(65.W)
  })

  val alu = Module(new freechips.rocketchip.rocket.ALU())

  alu.io.in1 := io.in1
  alu.io.in2 := io.in2
  alu.io.fn := io.fn
  io.out := alu.io.out

}

class VPU(implicit p: Parameters) extends BoomModule with HasVPUParameters with HasCoreParameters
{
  val io = IO(new Bundle {
    val req = Flipped(new ValidIO(new VpuReq))
    val resp = new ValidIO(new ExeUnitResp(65))
  })

  val io_req = io.req.bits

  val vec_decoder = Module(new UOPCodeVPUDecoder)
  vec_decoder.io.uopc := io_req.uop.uopc
  val vec_ctrl = vec_decoder.io.sigs
  // what is this??
  //val vec_rm = Mux(io_req.uop.fp_rm === 7.U, io_req.fcsr_rm, io_req.uop.fp_rm)

  // create all vector lane instances
  val vector_lanes: Seq[VectorLane] = (0 until 7) map { w =>
    Module(new VectorLane()).suggestName(s"vector_lane_${w}")
  }

  // connect request data to vector lanes
  var lane = 0
  for (vl <- vector_lanes) {
    var start_bit = lane * 32
    var end_bit = start_bit + 31

    vl.io.in1 := (io_req.rs1_data(start_bit, end_bit)).asUInt
    vl.io.in2 := (io_req.rs2_data(start_bit, end_bit)).asUInt
    lane += 1
  }

  // setting response
  io.resp.bits.uop          := DontCare
  io.resp.bits.predicated   := DontCare
  io.resp.bits.data         := DontCare         // was set in fpu
  io.resp.bits.fflags.valid := io.resp.valid
  io.resp.bits.fflags.bits  := DontCare         // was set in fpu
}