package swim.app

import java.util.Timer
import java.util.concurrent.TimeUnit

import com.google.common.math.LongMath
import fuel.func.RunExperiment
import fuel.util.IApp
import swim.app.Fibonacci.{penalizeForLackOfRecursiveCalls, worstPossibleFitness}
import swim.app.TestRecursiveDomain.nArgs
import swim.{Grammar, RecursiveDomain, Tests}
import swim.tree.{GPMoves, Op, SimpleGP}

import scala.collection.Seq
import scala.util.Random

case object Modu extends IApp('maxGenerations -> 100, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 2 // the number of input variables
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      '* -> ('I, 'I),
      '/ -> ('I, 'I),
//      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'rec -> ('I, 'I), // recursive call
      0L, 2L, 1L, // int constants
      'a -> ('A) // argument (input)
    ),
    'B -> Seq(
      true, false,
      'not -> ('B),
      'and -> ('B, 'B),
      'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  val numExamples = 12
  val maxRecursionDepth = numExamples + 1
  // 0-based indexing of series elements
  def modu(a: Long, b: Long): (Long) = a % b
  val tests = Tests(0.until(numExamples).flatMap(a => 1.until(numExamples).map(b => (a, b)))
    .map(n => (Seq(n._1.toLong, n._2.toLong), modu(n._1, n._2))))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeForLackOfRecursiveCalls = false
  val penalizeBigSolutions = true
  val maxSolutionSize = 11
  def eval(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else {
        val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
        val errSum = tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum;
        if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
        else errSum
      }
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    for (n <- tests.take(maxTest))
      println(n + " : " + domainTest(prog)(n._1))
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}
// ite (< (a(0), a(1)), a(0), rec (a(0) - a(1), a(1)))

case object Fact extends IApp('maxGenerations -> 100, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 1 // the number of input variables
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      '* -> ('I, 'I),
      '/ -> ('I, 'I),
      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'rec -> ('I), // recursive call
      /*0L, */2L, 1L, // int constants
      'a -> ('A) // argument (input)
    ),
    'B -> Seq(
      true, false,
      'not -> ('B),
      'and -> ('B, 'B),
      'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  val numExamples = 21
  val maxRecursionDepth = numExamples + 1
  // 0-based indexing of series elements
  def factorial(a: Long) : Long = if (a < 2) 1L else a * factorial(a-1)
  val tests = Tests(0L.until(numExamples).map(a => (Seq(a), factorial(a))))

  tests.foreach(t => println(t.input))
  tests.foreach(t => println(t.output))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeForLackOfRecursiveCalls = false
  val penalizeBigSolutions = false
  val maxSolutionSize = 14
  def eval(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else {
        val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
        val errSum = tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum;
        if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
        else errSum
      }
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    for (n <- tests.take(maxTest))
      println(n + " : " + domainTest(prog)(n.input))
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}
// ite(< (a(0) 2), 1, *(a(0) rec (-(a(0) 1))))

case object Maj5 extends IApp('maxGenerations -> 100, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 5 // the number of input variables
  val maxRecursionDepth = 0
  val grammar = Grammar('I,
    'I -> (Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      '* -> ('I, 'I),
      '/ -> ('I, 'I),
      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'rec -> ('I), // recursive call
      /*0L, 2L, 1L, 7L,*/// int constants
      'a -> ('A) // argument (input)
    ) ++ 0L.until(8L)),
    'B -> Seq(
      true, false,
      'not -> ('B),
      'and -> ('B, 'B),
      'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  // 0-based indexing of series elements
  def maj5(a: Seq[Long]) : Long = if ((a sum) > 7) 2L else 1L
  val tests = Tests(0L.until(1<<nArgs).map(a => 0L.until(nArgs).map(i => ((a>>i)&1)+1)).map(a => (a, maj5(a))))

  tests.foreach(t => println(t.input))
  tests.foreach(t => println(t.output))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeBigSolutions = false
  val maxSolutionSize = 19
  def eval(s: Op) = {
    val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
    val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
    if (outputs.size < tests.size) worstPossibleFitness
    else {
      val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
      val errSum = (tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum)
      if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
      else errSum
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    for (n <- tests)
      println(n + " : " + domainTest(prog)(n.input))
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}
// ite( >( +( +(a(0) +(a(1) a(2))) +(a(3) a(4))) 7L) 2L 1L)


case object Maj8 extends IApp('maxGenerations -> 300, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 8 // the number of input variables
  val maxRecursionDepth = 0
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
//      '- -> ('I, 'I),
//      '* -> ('I, 'I),
//      '/ -> ('I, 'I),
//      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
//      'rec -> ('I), // recursive call
      /*0L, */2L, 1L, 12L,// int constants
      'a -> ('A) // argument (input)
    ),
    'B -> Seq(
//      true, false,
//      'not -> ('B),
//      'and -> ('B, 'B),
//      'or -> ('B, 'B),
//      '< -> ('I, 'I),
      '> -> ('I, 'I)//,
//      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  // 0-based indexing of series elements
  def maj8(a: Seq[Long]) : Long = if ((a sum) > 12) 2L else 1L
  val tests = Tests(Random.shuffle(0L.until(1<<nArgs).map(a => 0L.until(nArgs).map(i => ((a>>i)&1)+1)).map(a => (a, maj8(a)))))

  tests.foreach(t => println(t.input))
  tests.foreach(t => println(t.output))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeBigSolutions = false
  val maxSolutionSize = 19
  def eval(s: Op) = {
    val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
    val outputs = tests.toStream.take(128).map(test => execContext(test.input)).takeWhile(_.isDefined)
    if (outputs.size < tests.take(128).size) worstPossibleFitness
    else {
      val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
      val errSum = (tests.take(128).zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum)
      if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
      else errSum
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    for (n <- tests)
      println(n + " : " + domainTest(prog)(n.input))
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}

case object Max4 extends IApp('maxGenerations -> 100, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 4 // the number of input variables
  val nExamples = 200
  val maxRecursionDepth = 0
  val rand = new Random(28598459837L)
  val grammar = Grammar('I,
    'I -> Seq(
//      '+ -> ('I, 'I),
//      '- -> ('I, 'I),
//      '* -> ('I, 'I),
//      '/ -> ('I, 'I),
//      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
//      'rec -> ('I), // recursive call
      /*0L, 2L, 1L,*/// int constants
      'a -> ('A) // argument (input)
    ),
    'B -> Seq(
//      true, false,
      'not -> ('B),
      'and -> ('B, 'B),
      'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  // 0-based indexing of series elements
  def max4(a: Seq[Long]) : Long = a.max
  val tests = Tests(0L.until(nExamples).map(a => 0.until(4).map(b => Math.abs(rand.nextLong() % 100L))).map(a => (a, max4(a))))

  tests.foreach(t => println(t.input))
  tests.foreach(t => println(t.output))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeBigSolutions = true
  val maxSolutionSize = 58
  def eval(s: Op) = {
    val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
    val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
    if (outputs.size < tests.size) worstPossibleFitness
    else {
      val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
      val errSum = (tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum)
      if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
      else errSum
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    println("validating for extra tests")
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    val prog = bsf._1
    var isValid = true
    val failedTests = 0L.until(nExamples).map(a => 0.until(4).map(b => Math.abs(rand.nextLong() % 100L)))
      .map(a => {
        val expectedAnswer = max4(a)
        val solutionAnswer = domainTest(prog)(a)
        (a, (solutionAnswer, expectedAnswer))
      })
      .filter(a => !a._2._1.contains(a._2._2))
      .take(1)

    if (failedTests.length > 0L) {
      failedTests.foreach(f => printf("Expected %d for %s, but found %s%n", f._2._2, f._1, f._2._1))
    } else {
      println("All extra tests passed")
      val maxTest = 20
      for (n <- tests)
        println(n + " : " + domainTest(prog)(n.input))
    }
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}
// ite a(0) > a(1)
//   ite a(0) > a(2)
//     ite a(0) > a(3)
//       a(0)
//       a(3)
//     ite a(2) > a(3)
//       a(2)
//       a(3)
//   ite a(1) > a(2)
//     ite a(1) > a(3)
//       a(1)
//       a(3)
//     ite a(2) > a(3)
//       a(2)
//       a(3)
// ite( (a3 == a0 || (a2 > a1 && a2 > a3))
//    ite(
//      >(
//        a(0)
//        a(2))
//      a(0)
//      a(2))
//    ite(
//      <(
//        ite(
//          >(
//            a(1)
//            a(3))
//          a(1)
//          a(3))
//        a(0))
//      a(0)
//      ite(
//        >(
//          a(1)
//          a(3))
//        a(1)
//        a(3))))

case object Fibonacci
  extends IApp('maxGenerations -> 100, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 1 // the number of input variables
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      //      '* -> ('I, 'I),
      //'/ -> ('I, 'I),
      //'% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'a -> ('A), // argument (input)
      'rec -> ('I), // recursive call
      2L, 1L//, // int constants
    ),
    'B -> Seq(
      //true, false,
      //'not -> ('B),
      //'and -> ('B, 'B),
      //'or -> ('B, 'B),
            '< -> ('I, 'I)//,
      //      '> -> ('I, 'I),
//      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  val numExamples = 12
  // 0-based indexing of series elements
  def fibonacci(n: Long): Long = if (n <= 1) 1 else fibonacci(n - 1) + fibonacci(n - 2)
  val tests = Tests(0.until(numExamples).map(n => (Seq(n.toLong), fibonacci(n))))

  val domain = RecursiveIntDomain(nArgs, numExamples + 1 /* recursion depth limit */ )

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeForLackOfRecursiveCalls = true
  def eval(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)
  val startTime = System.currentTimeMillis()
  RunExperiment(alg)
  val elapsedTime = System.currentTimeMillis() - startTime;

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxTest = 20
    val domainTest = RecursiveIntDomain(nArgs, maxTest)
    for (n <- 0.until(maxTest))
      println(n + " : " + domainTest(prog)(Seq(n)))
  }
  printf("Elapsed time: %d%n", elapsedTime)
}

case object IntSqrt extends IApp('maxGenerations -> 300, 'populationSize -> 10000, 'parEval -> true) {

  val nArgs = 1 // the number of input variables
  val nExamples = 20
  val maxRecursionDepth = 0
  val rand = new Random(28598459837L)
  val grammar = Grammar('I,
    'I -> Seq(
      '+ -> ('I, 'I),
      '- -> ('I, 'I),
      '* -> ('I, 'I),
      '/ -> ('I, 'I),
      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'rec -> ('I), // recursive call
      /*0L, 2L, */1L,// int constants
      'a -> ('A) // argument (input)
    ),
    'B -> Seq(
//      true, false,
      'not -> ('B),
      'and -> ('B, 'B),
      'or -> ('B, 'B),
      '< -> ('I, 'I),
      '> -> ('I, 'I),
      '== -> ('I, 'I)
    ),
    'A -> 0.until(nArgs)) // argument index

  // 0-based indexing of series elements
  def intSqrt(a: Long) : Long = Math.floor(Math.sqrt(a)).asInstanceOf[Long]
  val tests = Tests(1L.until(nExamples + 1).map(a => (Seq(a), intSqrt(a))))

  tests.foreach(t => println(t.input))
  tests.foreach(t => println(t.output))

  val domain = RecursiveIntDomain2(nArgs, maxRecursionDepth)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue
  def errAbsolute(a: Long, b: Long) = math.abs(a - b)
  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1
  val err = errHamming _

  val penalizeForLackOfRecursiveCalls = true
  val numberOfRecursions = 4
  val penalizeBigSolutions = false
  val maxSolutionSize = 37
  def eval(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else {
        val sizePenalization : Long = if (penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize) else 0
        val errSum = (tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum)
        if (errSum == 0 && penalizeBigSolutions) Math.max(0, s.size - maxSolutionSize).toLong
        else errSum
      }
    }
  }

  def correctDiscrete = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible),
    eval, correctDiscrete)

  val t0 = System.nanoTime()
  RunExperiment(alg)
  val t1 = System.nanoTime()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    println("validating for extra tests")
    val domainTest = RecursiveIntDomain(nArgs, maxRecursionDepth)
    val prog = bsf._1
    var isValid = true
    val failedTests = 20L.until(25L)
      .map(a => {
        val expectedAnswer = intSqrt(a)
        val solutionAnswer = domainTest(prog)(Seq(a))
        (a, (solutionAnswer, expectedAnswer))
      })
      .filter(a => !a._2._1.contains(a._2._2))
      .take(1)

    if (failedTests.length > 0L) {
      failedTests.foreach(f => printf("Expected %d for %s, but found %s%n", f._2._2, f._1, f._2._1))
    } else {
      println("All extra tests passed")
      val maxTest = 20
      for (n <- tests)
        println(n + " : " + domainTest(prog)(n.input))
    }
  }
  printf("Time processing: %d%n", TimeUnit.NANOSECONDS.toMillis(t1 - t0))
}
// ite(
//   ==(
//     a(0)
//     1)
//   1
//   ite(
//     >(
//       *(
//         +(
//           rec(-(1 a(0)))
//           1)
//         +(
//           rec(-(1 a(0)))
//           1))
//       a(0))
//     rec(-(1 a(0)))
//     +(
//       rec(-(1 a(0)))
//       1)))
//

case object GCD extends IApp('maxGenerations -> 100, 'populationSize -> 1000, 'parEval -> true) {
  val numberOfArguments = 2
  val grammar = Grammar('I,
    'I -> Seq(
//      '+ -> ('I, 'I),
//      '- -> ('I, 'I),
//      '* -> ('I, 'I),
//      '/ -> ('I, 'I),
      '% -> ('I, 'I),
      'ite -> ('B, 'I, 'I),
      'a -> ('A), // argument (input)
      'rec -> ('I, 'I), // recursive call
      0L//, 1L, 2L //, // int constants
    ),
    'B -> Seq(
//      true, false,
//      'not -> ('B),
//      'and -> ('B, 'B),
//      'or -> ('B, 'B),
//      '< -> ('I, 'I),
//      '> -> ('I, 'I),
      '== -> ('I, 'I)),
    'A -> Seq(0, 1)) // number of arguments

  def genGCDTest = (n: Long, range: (Long, Long)) => {
    val diff = (range._2 - range._1) + 1
    val inputA = range._1 + (n % diff)
    val inputB = range._2 + (n / diff)
    val gcd = LongMath.gcd(inputA, inputB)
    (Seq(inputA, inputB), gcd)
  }

  val testsRange = (5L, 100L)
  val cases = testsRange._1.until(testsRange._2)
    .flatMap{case a => testsRange._1.until(testsRange._2).map{case b => (a, b)}}
    .map{case (a, b) => (Seq(a, b), LongMath.gcd(a, b))}
    .filter(i => i._2 != 1) // gcd != 1
//    .filter(i => Set(i._1).size == 2) // a !=  b
  printf("Number of test cases: %d%n", cases.length)
  val numberOfTests = cases.size
  val domain = RecursiveIntDomain(numberOfArguments, 15 /* recursion depth limit */)
  val tests = Tests(cases)

  // Evaluation function (fitness):
  val worstPossibleFitness = Long.MaxValue

  def errAbsolute(a: Long, b: Long) = math.abs(a - b)

  def errHamming(a: Long, b: Long) = if (a == b) 0 else 1

  val err = errHamming _
  val penalizeForLackOfRecursiveCalls = true

  def fitnessFunction(s: Op) = {
    if (penalizeForLackOfRecursiveCalls && !s.contains('rec)) worstPossibleFitness
    else {
      val execContext = domain(s) _ // TODO: Introduce this in other Swim Examples
      val outputs = tests.toStream.map(test => execContext(test.input)).takeWhile(_.isDefined)
      if (outputs.size < tests.size) worstPossibleFitness
      else tests.zip(outputs).map { case (test, actualOut) => err(actualOut.get, test.output).toLong } sum
    }
  }

  val stopFunction = (_: Any, e: Long) => e == 0

  val alg = new SimpleGP(GPMoves(grammar, SimpleGP.defaultFeasible), fitnessFunction, stopFunction)
  val startTime = System.currentTimeMillis()
  RunExperiment(alg)
  val endTime = System.currentTimeMillis()

  // For successful runs, show how/whether the best-of-run generalizes
  val bsf = alg.bsf.bestSoFar.get
  if (bsf._2 == 0) {
    val prog = bsf._1
    val maxPrint = 20
    for (n <- cases.indices) {
      val (input, output) = cases(n)
      val testOutput = domain(prog)(input)
      if (n < maxPrint) println(input + " : " + testOutput + " " + output)
      assume(testOutput.contains(output))
    }
  }
  printf("%d%n", endTime - startTime)
}


case class RecursiveIntDomainWithTuple(override val numVars: Int, override val recDepthLimit: Int, recursionSymbol: Symbol = 'rec)
  extends RecursiveDomain[Long, (Long, Long)](numVars, recDepthLimit, recursionSymbol) {
  override def operationalSemantics(input: Seq[Long])(childRes: Seq[Any]): Any = {
    childRes match {
      case Seq('+, a: Long, b: Long)         => a + b
      case Seq('-, a: Long, b: Long)         => a - b
      case Seq('*, a: Long, b: Long)         => a * b
      case Seq('/, a: Long, b: Long)         => if (b != 0) a / b else 1L
      case Seq('%, a: Long, b: Long)         => if (b != 0) a % b else 1L

      case Seq('<, a: Long, b: Long)         => a < b
      case Seq('>, a: Long, b: Long)         => a > b
      case Seq('==, a: Long, b: Long)        => a == b

      case Seq('and, a: Boolean, b: Boolean) => a && b
      case Seq('or, a: Boolean, b: Boolean)  => a || b
      case Seq('not, a: Boolean)             => !a

      case Seq('<>, a: Long, b: Long)        => (a, b)
      case Seq('_1, a: (Long, Long))         => a._1
      case Seq('_2, a: (Long, Long))         => a._2

      case Seq('a, a: Int)                   => input(a)
      case Seq(b: Boolean)                   => b
      case Seq(v: Long)                      => v
      case Seq(v: Int)                       => v
    }
  }
}
