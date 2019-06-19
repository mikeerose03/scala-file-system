package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command extends (State => State) {
  def apply(state: State): State
}

object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val ECHO = "echo"
  val CAT = "cat"

  def empty: Command = new Command {
    override def apply(state: State): State = state
  }

  def incomplete(name: String): Command = new Command {
    override def apply(state: State): State = state.setMessage(s"$name is an incomplete command")
  }

  def from(input: String): Command = {
    val tokens: Array[String] = input.split(" ")
    if (input.isEmpty || tokens.isEmpty) empty
    else tokens(0) match {
      case MKDIR =>
        if (tokens.length < 2) incomplete(MKDIR)
        else new Mkdir(tokens(1))
      case LS => new Ls
      case PWD => new Pwd
      case TOUCH => new Touch(tokens(1))
      case CD =>
        if (tokens.length < 2) incomplete(MKDIR)
        else new Cd(tokens(1))
      case RM =>
        if (tokens.length < 2) incomplete(RM)
        else new Rm(tokens(1))
      case ECHO =>
        if (tokens.length < 2) incomplete(ECHO)
        else new Echo(tokens.tail)
      case CAT =>
        if (tokens.length < 2) incomplete(CAT)
        else new Cat(tokens(1))
      case _ => new UnknownCommand
    }

  }

}