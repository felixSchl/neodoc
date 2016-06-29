module Examples where

import Prelude
import Test.Spec (Spec(), describe, it)

examples = \_ -> describe "examples" do
 it "can parse values" do

--------------------------------------------------------------------------------
------ Parsing values ----------------------------------------------------------
--------------------------------------------------------------------------------

  let help = """
    usage: git [--version] [--help] [-C <path>] [-c <name=value>]
              [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
              [-p|--paginate|--no-pager] [--no-replace-objects] [--bare]
              [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
              [<command> [<args>]...]
  """

  pure unit
