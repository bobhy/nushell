use std::collections::HashMap;

use crate::{engine::VarInfo, BlockId, VarId};

#[derive(Clone, Debug)]
pub struct Closure {
    pub block_id: BlockId,
    pub captures: HashMap<VarId, VarInfo>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub block_id: BlockId,
}
