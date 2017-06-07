
module Graph where
import AST (Pos, Type, Unop, Binop)

type NodeID = Integer
data NodeLabel = NodeLabel
    { node_id    :: NodeID
    , node_pos   :: Pos
    , node_label :: Maybe String -- If their was an explicit label
                                 -- in the source code
    , node_out   :: [EdgeID]
    , node_in    :: [EdgeID]
    }

type EdgeID = Integer
data EdgeLabel = EdgeLabel
    { edge_id   :: EdgeID
    , edge_src  :: NodeID
    , edge_dst  :: NodeID
    , edge_inst :: EdgeInst
    }

-- We accepts variables of type void, but their evaluation result
-- in a bottom expression
type EVarID = Integer
data EdgeVar = EdgeVar
    { edge_var_id   :: EVarID
    , edge_var_name :: String
    , edge_var_type :: Type ()
    , edge_var_pos  :: Pos
    }

data EdgeExpr =
      EEconst Integer
    | EEvar   EVarID
    | EInter  EdgeExpr EdgeExpr -- Support for random integers in interval
    | EEbinop (Binop Pos) EdgeExpr EdgeExpr
    | EEunop  (Unop  Pos) EdgeExpr

type FunID = Integer
data Function = Function
    { function_id       :: FunID
    , function_name     :: String
    , function_entry    :: NodeID
    , function_exit     :: NodeID
    , function_params   :: [EVarID]
    , function_ret      :: EVarID -- If function doesn't return anything,
                                  -- function_ret must be of type void
    , function_ret_type :: Type ()
    }

data EdgeInst =
      EIassign EVarID EdgeExpr
    | EInop    -- May be necessary sometimes
    | EIcall   FunID
    | EIguard  EdgeExpr
    | EIassert EdgeExpr

data Program = Program
    { program_vars :: [EdgeVar]
    , program_nodes :: [NodeLabel]
    , program_edges :: [EdgeLabel]
    , program_init_entry :: NodeID
    , program_init_exit  :: NodeID
    , program_functions  :: [Function]
    }

