package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {

  override def apply(state: State): State = {
    /*
      if no args, state
      else if just one arg, print to console,
      else if multiple args,
        if next to last args is >
          echo to a file
        if >> append to file
        else
          echo everything tho console
     */


    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length-2)
      val filename = args(args.length-1)
      val contents = createContent(args, args.length-2)

      if (">>".equals(operator))
        doEcho(state, contents, filename, true)
      else if(">".equals(operator))
        doEcho(state, contents, filename, false)
      else
        state.setMessage(createContent(args, args.length))
    }
  }

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, appendMode: Boolean): Directory = {
    /*
      if path.isEmpty fail (return currentDirectory)
      else if path.tail.isEmpty
        find the file to add content to
        if file not find
          create file
        else if is not file, then fail
        else
          append content to file
          replace the entry with the filename with the NEW appended FILE
       else
        find the next directory to navigate
        call getRootAfterEcho recursively

        if recursuve call failed, then fail
        else replace entry with the NEW directory after the recursive call
     */
    if (path.isEmpty) currentDirectory
    else if (path.tail.isEmpty) {
      val dirEntry = currentDirectory.findEntry(path.head)

      if (dirEntry == null)
        currentDirectory.addEntry(new File(currentDirectory.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDirectory
      else {
        if (appendMode) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
      }
    } else {
      val nextDir = currentDirectory.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir, path.tail, contents, appendMode)

      if (newNextDir == nextDir) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDir)
    }
  }

  def doEcho(state: State, contents: String, filename: String, appendMode: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo filename must not contain separators.")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, appendMode)
      if (newRoot == state.root)
        state.setMessage(s"$filename does not exist.")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }

  // top index non inclusive
  def createContent(args: Array[String], topIndex: Int): String  = {
    @tailrec
    def createContentHelper(currentIndex: Int, accumulator: String): String = {
      if (currentIndex >= topIndex) accumulator
      else createContentHelper(currentIndex+1, accumulator + " " + args(currentIndex))
    }

    createContentHelper(0, "")
  }
}
