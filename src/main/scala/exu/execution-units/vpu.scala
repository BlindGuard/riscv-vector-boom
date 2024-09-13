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
class VpuReq()(implicit p: Parameters) extends BoomBundle with HasCoreParameters
{
  val uop      = new MicroOp()
  val rs1_data = Bits(vLen.W)
  val rs2_data = Bits(vLen.W)
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

/**
  * Class representing a single vector lane.
  * One lane executes an arithmetic operation on part of the vector 
  * that was put into the VPU.
  *
  * @param laneIndex Identifier of the lane
  */
class VectorLane(val laneIndex: Int)(implicit p: Parameters) extends BoomModule with HasVPUParameters
{
  val io = IO(new Bundle {
    val in1 = Input(UInt(xLen.W))
    val in2 = Input(UInt(xLen.W))
    val fn = Input(UInt((new freechips.rocketchip.rocket.ALUFN).SZ_ALU_FN.W))

    val out = Output(UInt(xLen.W))
    val tag = UInt(8.W)
  })

  io.tag := laneIndex.U
  val alu = Module(new freechips.rocketchip.rocket.ALU())

  alu.io.in1 := io.in1
  alu.io.in2 := io.in2
  alu.io.fn := io.fn
  io.out := alu.io.out

}

// not used, remove!
class QueueEntry(implicit p: Parameters) extends BoomBundle()(p)
{
  val tag       = UInt(8.W)                       // from which vector lane is the addr
  val addr      = Valid(UInt(coreMaxAddrBits.W))  // the address that should be loaded
  val executed  = Bool()                          // true when request was sent
  val data      = UInt(coreDataBits.W)            // loaded data
  val loaded    = Bool()                          // true when data is loaded

  val req = new rocket.HellaCacheReq()
}

class VPU(implicit p: Parameters) extends BoomModule with HasVPUParameters with HasCoreParameters
{
  val io = IO(new Bundle {
    val req = Flipped(new ValidIO(new VpuReq))

    val resp = new ValidIO(new ExeUnitResp(vLen))

    val mem = new rocket.HellaCacheIO
  })

  val io_req = io.req.bits
  val lanes = 8

  val vec_decoder = Module(new UOPCodeVPUDecoder)
  vec_decoder.io.uopc := io_req.uop.uopc
  val vec_ctrl = vec_decoder.io.sigs

  // create all vector lane instances
  val vector_lanes: Seq[VectorLane] = (0 until lanes) map { w =>
    Module(new VectorLane(w)).suggestName(s"vector_lane_${w}")
  }

  // internal buffer for memory requests
  val queue = Reg(Vec(lanes, new DecoupledIO(new rocket.HellaCacheReq())))

  // arbiter for the cache IO
  val mem_arb = Module(new Arbiter(new rocket.HellaCacheReq(), lanes))
  io.mem.req <> mem_arb.io.out

  var lane = 0
  for (vl <- vector_lanes) {
    var start_bit = lane * 32
    var end_bit = start_bit + 31

    // connect request data to vector lanes
    vl.io.in1 := (io_req.rs1_data(end_bit, start_bit)).asUInt
    vl.io.in2 := (io_req.rs2_data(end_bit, start_bit)).asUInt
    
    // connect lane output to the "queue"
    queue(lane).bits.addr := vl.io.out
    queue(lane).bits.tag := vl.io.tag

    // connect queue to arbiter
    mem_arb.io.in(lane) <> queue(lane)

    // set ALU function as ADD hardcoded
    vl.io.fn := (new freechips.rocketchip.rocket.ALUFN).FN_ADD
    
    lane += 1
  }

  // collect responses and build vector from them
  val mem_resp = Reg(Vec(lanes, Valid(new rocket.HellaCacheResp())))
  mem_resp(io.mem.resp.bits.tag) := io.mem.resp

  // when all responses are valid,
  // all data was loaded from memory
  // we just need to assemble it into a vector 
  // and set it as data in the ExeUnitResp
  when(mem_resp(0).bits.has_data &&
       mem_resp(1).bits.has_data &&
       mem_resp(2).bits.has_data &&
       mem_resp(3).bits.has_data &&
       mem_resp(4).bits.has_data &&
       mem_resp(5).bits.has_data &&
       mem_resp(6).bits.has_data &&
       mem_resp(7).bits.has_data )
  {
    io.resp.bits.data := Cat(Seq(mem_resp(0).bits.data, mem_resp(1).bits.data, mem_resp(2).bits.data, mem_resp(3).bits.data, mem_resp(4).bits.data, mem_resp(5).bits.data, mem_resp(6).bits.data, mem_resp(7).bits.data))

    // for (i <- 0 until lanes) 
    // {
    //   var start_bit = lane * 32
    //   var end_bit = start_bit + 31

    //   io.resp.bits.data(end_bit, start_bit) := mem_resp(i).bits.data
    // }

    io.resp.bits.uop := io.req.bits.uop
    io.resp.valid := true.B
  }

  // default values for the response
  io.resp.bits.uop          := DontCare
  io.resp.bits.predicated   := DontCare
  io.resp.bits.data         := DontCare
  io.resp.bits.fflags.valid := io.resp.valid
  io.resp.valid             := false.B
  io.resp.bits.fflags.bits  := DontCare         // was set in fpu
}