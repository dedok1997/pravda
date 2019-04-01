package pravda.evm.disasm
import pravda.evm.EVM._
import pravda.evm.disasm.Blocks.{WithJumpDest, WithJumpI}

object SymbolicExecutor {

  case class SymbolicExecutionResult(jumps: Set[Jump], jumpis: Set[JumpI], path: List[Op])

  def evalOp(op: Op, state: StackWithNegativeSize[BigInt]): StackWithNegativeSize[BigInt] = {
    op match {
      case Push(n)                   =>
        state push BigInt(1, n.toArray)
      case Swap(n) => state swap n
      case Dup(n)  => state dup n
      case And if state.pop()._1.get >= 0 && state.pop()._2.pop()._1.get >= 0    =>
        val (Some(x), t) = state.pop()
        val (Some(y), res) = t.pop()
        res.push(x & y)

      case op =>
        val r = OpCodes.stackReadCount(op)
        val (args, s) = state.pop(r)
        val w = OpCodes.stackWriteCount(op)
        val resState = (1 to w).foldLeft(s) { case (state, n) => state push BigInt(-n) }
        resState
    }
  }

  def eval(ops: Vector[Op]): Set[TargetedJumpOp] = {
    val jumpdest = ops.zipWithIndex.collect { case t @ (JumpDest(addr), ind) => addr -> ind }.toMap

    var used = Set.empty[(Int,Int)]
    var usedI = Set.empty[Int]

    def eval(stack: StackWithNegativeSize[BigInt])(i: Int,
                                                   acc: SymbolicExecutionResult): List[SymbolicExecutionResult] = {
      usedI = usedI + i
      ops(i) match {

        case SelfAddressedJump(addr)  =>
          val (Some(toBI), state) = stack.pop()
          val to = toBI.intValue()
          if (jumpdest.contains(to)) {
            val ind = jumpdest(to)
            if (!used.contains(addr -> to)) {
              used = used + (addr -> to)
              eval(state)(ind, acc.copy(jumps = acc.jumps + Jump(addr, to)))
            } else List(acc)
          } else List(acc)

        case SelfAddressedJumpI(addr) =>
          val (Some(toBI), tail) = stack.pop()
          val to = toBI.intValue()
          val (_, r) = tail.pop()
          val ind = jumpdest(to.intValue())
          if (!used.contains(addr -> to)) {
            used = used + (addr -> to)
            eval(r)(ind, acc.copy(jumpis = acc.jumpis + JumpI(addr, to))) ++ eval(r)(i + 1, acc)
          } else List(acc.copy(jumpis = acc.jumpis + JumpI(addr, to))) ++ eval(r)(i + 1, acc)

        case op if OpCodes.terminate(op)=>
          List(acc)

        case op =>
          val state = evalOp(op, stack)
          eval(state)(i + 1, acc)
      }
  }
    val r = eval(StackList.empty)(0, SymbolicExecutionResult(Set.empty, Set.empty, Nil))
    r.flatMap(t => t.jumps ++ t.jumpis).toSet
  }

  def eval(main: List[List[Op]],
           withJumpDest: Map[Int, WithJumpDest],
           withJumpI: Map[Int, WithJumpI]): Set[TargetedJumpOp] = {

    def eval(stack: StackWithNegativeSize[BigInt])(block: List[Op],
                                                   withJumpDest: Map[Int, WithJumpDest],
                                                   withJumpI: Map[Int, WithJumpI],
                                                   acc: SymbolicExecutionResult,
                                                   used: Set[Int]): List[SymbolicExecutionResult] = {
      val (st, lastOp) = block.foldLeft(stack -> Option.empty[Op]) {
        case ((stack, lastOp), op) =>
          op match {
            case SelfAddressedJump(addr) if stack.size > 0 =>
              val (Some(dest), r) = stack.pop()
              r -> Some(Jump(addr, dest.intValue()))
            case SelfAddressedJumpI(addr) if stack.size > 1 =>
              val (Some(dest), r) = stack.pop()
              val (_, res) = r.pop()
              res -> Some(JumpI(addr, dest.intValue()))
            case _ => evalOp(op, stack) -> lastOp
          }
      }

      lastOp match {

        case Some(j @ Jump(addr, dest)) if withJumpDest.contains(dest) && !used.contains(dest) =>
          eval(st)(
            withJumpDest(dest).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumps = acc.jumps + j, path = acc.path ++ block),
            used + dest
          )

        case Some(j @ JumpI(addr, dest)) if withJumpDest.contains(dest) && !used.contains(dest) =>
          val ex1 = eval(st)(
            withJumpDest(dest).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumpis = acc.jumpis + j, path = acc.path ++ block),
            used + dest
          )
          val ex2 = eval(st)(
            withJumpI(addr).ops,
            withJumpDest,
            withJumpI,
            acc.copy(jumpis = acc.jumpis + j, path = acc.path ++ block),
            used + dest
          )
          ex1 ++ ex2

        case _ => List(acc)
      }
    }

    val initStack = new StackList(List.fill(100)(BigInt(-1)),100)

    val r = main.foldLeft(List.empty[SymbolicExecutionResult]) {
      case (acc, block) =>
        acc ++ eval(initStack)(block,
                                     withJumpDest,
                                     withJumpI,
                                     SymbolicExecutionResult(Set.empty, Set.empty, Nil),
                                     Set.empty)
    }

    val r1 = withJumpDest.foldLeft(List.empty[SymbolicExecutionResult]) {
      case (acc, block) =>
        acc ++ eval(initStack)(block._2.ops,
                                     withJumpDest,
                                     withJumpI,
                                     SymbolicExecutionResult(Set.empty, Set.empty, Nil),
                                     Set.empty)
    }

    val r2 = (r.map(res => res.jumps -> res.jumpis) ++ r1.map(res => res.jumps -> res.jumpis))
      .foldLeft(Set.empty[Jump] -> Set.empty[JumpI]) {
        case ((s1, s2), (s3, s4)) =>
          (s1 ++ s3) -> (s2 ++ s4)
      }

    val x: Set[TargetedJumpOp] = r2._1 ++ r2._2
    x
  }

  def jumps(blocks: List[List[Op]]): Set[TargetedJumpOp] = {
    val jumpable = Blocks.jumpable(blocks)
    val main = jumpable.withoutJumpdest.filter {
      case SelfAddressedJumpI(_) :: _ => false
      case _                          => true
    }
    val byJumpdest = jumpable.withJumpdest.groupBy(_.dest).map { case (k, v)     => k.addr -> v.head }
    val byJumpi = Blocks.continuation(blocks).groupBy(_.jumpi).map { case (k, v) => k.addr -> v.head }
    eval(main, byJumpdest, byJumpi)
  }
}
