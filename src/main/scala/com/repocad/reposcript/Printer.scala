package com.repocad.reposcript

import com.repocad.reposcript.parsing._

/**
 * A printer that can print objects on a medium
 */
trait Printer[T] {

  val context : T
  
  var actions = Seq[T => Unit]()

  lazy val toEvaluatorEnv : evaluating.Env = {
    Map(
      "arc"  -> ((env : evaluating.Env, x : Double, y : Double, r : Double, sAngle : Double, eAngle : Double) => arc(x, y, r, sAngle, eAngle)),
      "bezier" -> ((env : evaluating.Env, x1 : Double, y1 : Double, x2 : Double, y2 : Double, x3 : Double, y3 : Double, x4 : Double, y4 : Double) => bezierCurve(x1, y1, x2, y2, x3, y3, x4, y4)),
      "circle" -> ((env : evaluating.Env, x : Double, y : Double, r : Double) => circle(x, y, r)),
      "line" -> ((env : evaluating.Env, x1 : Double, y1 : Double, x2 : Double, y2 : Double) => line(x1, y1, x2, y2)),
      "text" -> ((env : evaluating.Env, x : Double, y : Double, h : Double, t : Any) => text(x, y, h, t))
    )
  }
  
  def addAction(action : T => Unit): Unit = {
    actions :+= action
  }

  /**
   * Draws an arc
   * @param x First coordinate
   * @param y Second coordinate
   * @param r Radius
   * @param sAngle start angle (3'o clock)
   * @param eAngle end angle
   */
  def arc(x : Double, y : Double, r : Double, sAngle : Double, eAngle : Double)

  /**
   * Draws a bezier curve
   * @param x1 start x
   * @param y1 start y
   * @param x2 control point1 x
   * @param y2 control point1 y
   * @param x3 control point2 x
   * @param y3 control point2 y
   * @param x4 end x
   * @param y4 start y
   */
  def bezierCurve(x1 : Double, y1 : Double, x2 : Double, y2 : Double, x3 : Double, y3 : Double, x4 : Double, y4 : Double)

  /**
   * Draws a circle
   * @param x First coordinate
   * @param y Second coordinate
   * @param r Radius
   */
  def circle(x : Double, y : Double, r : Double) : Unit

  /**
   * Draws a paper
   */
  protected def drawPaper() : Unit

  def execute(): Unit = {
    drawPaper()
    actions.foreach(_.apply(context))
  }

  /**
   * Draws a line
   * @param x1 First coordinate
   * @param y1 Second coordinate
   * @param x2 Third coordinate
   * @param y2 Fourth coordinate
   */
  def line(x1 : Double, y1 : Double, x2 : Double, y2 : Double)

  /**
   * Renders a text string
   * @param x First coordinate
   * @param y Second coordinate
   * @param h Height
   * @param t Text
   */
  def text(x : Double, y : Double, h : Double, t : Any)

  /**
   * Prepares the printer for drawing.
   */
  def prepare() : Unit

}

object Printer {

  lazy val emptyEvaluatorEnv : evaluating.Env = Map(
    "arc"  -> ((env : evaluating.Env, a : Double, b : Double, c : Double, d : Double, e : Double) => Unit),
    "bezier" -> ((env : evaluating.Env, a : Double, b : Double, c : Double, d : Double, e : Double, f : Double) => Unit),
    "circle" -> ((env : evaluating.Env, a : Double, b : Double, c : Double) => Unit),
    "line" -> ((env : evaluating.Env, a : Double, b : Double, c : Double, d : Double) => Unit),
    "text" -> ((env : evaluating.Env, a : Double, b : Double, c : Double, d : Any) => Unit)
  )

  lazy val toParserEnv : parsing.ValueEnv = Map(
      "arc"  -> FunctionExpr("arc", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("r", NumberType), RefExpr("sAngle", NumberType), RefExpr("eAngle", NumberType)), UnitExpr),
      "bezier" -> FunctionExpr("bezier", Seq(RefExpr("x1", NumberType), RefExpr("y1", NumberType), RefExpr("x2", NumberType), RefExpr("y2", NumberType), RefExpr("x3", NumberType), RefExpr("y3", NumberType), RefExpr("x4", NumberType), RefExpr("y4", NumberType)), UnitExpr),
      "circle" -> FunctionExpr("circle", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("r", NumberType)), UnitExpr),
      "line" -> FunctionExpr("line", Seq(RefExpr("x1", NumberType), RefExpr("y1", NumberType), RefExpr("x2", NumberType), RefExpr("y2", NumberType)), UnitExpr),
      "text" -> FunctionExpr("text", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("h", NumberType), RefExpr("t", AnyType)), UnitExpr)
    )

}