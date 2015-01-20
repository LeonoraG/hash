-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
module Hash.Language.Commands where

import Exec
import qualified Data.Map as M


--commands :: Data.Map String Command

type Dict = M.Map String String
commands :: Dict

commands = M.fromList [("rm","remove"),("lst","list")]

