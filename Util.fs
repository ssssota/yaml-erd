module Util

open System

type Warning =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      Message: string }

let warningToConsoleString w =
    String.Format
        ("\x1b[33m warning[{0}:{1}-{2}:{3}]: \x1b[0m {4}", w.StartLine, w.StartColumn, w.EndLine, w.EndColumn, w.Message)

type ErrorValue =
    { StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      Message: string }

let errorToConsoleString e =
    String.Format
        ("\x1b[31m error[{0}:{1}-{2}:{3}]: \x1b[0m {4}", e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

type OkValue<'a> = { Data: 'a; Warnings: Warning list }
type Result<'a> = Result<OkValue<'a>, ErrorValue list>
