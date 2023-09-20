use serde::Deserialize;
use serde::Serialize;

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Expression};
use crate::engine::EngineState;
use crate::engine::DEFAULT_OVERLAY_NAME;
use crate::{ShellError, Span, Value, VarId, Variable};

/// Environment variables per overlay
pub type EnvVars = HashMap<String, HashMap<String, Value>>;

// runtime information about a VarId
// now including information about the expression the variable came from.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarInfo {
    pub var_id: VarId,
    pub value: Value,
    pub expr: Option<Expr>, //
    pub span: Option<Span>,
}

impl VarInfo {
    pub fn new(var_id: VarId, value: Value, expression: Option<&Expression>) -> Self {
        if let Some(e) = expression {
            VarInfo {
                var_id,
                value,
                expr: Some(e.expr.clone()),
                span: Some(e.span),
            }
        } else {
            VarInfo {
                var_id,
                value,
                expr: None,
                span: None,
            }
        }
    }
    pub fn new_from_const(var_id: VarId, const_val: Value, variable: &Variable) -> Self {
        VarInfo {
            var_id,
            value: const_val,
            expr: Some(Expr::Bool(false)), // this is an egregious lie, but may not cause any confusion!
            // So far, the only use is to compare to Expr::Var to stop coercion for metadata
            /// and the declaration_span is presumably correct.
            span: Some(variable.declaration_span),
        }
    }
}

/// A runtime value stack used during evaluation
///
/// A note on implementation:
///
/// We previously set up the stack in a traditional way, where stack frames had parents which would
/// represent other frames that you might return to when exiting a function.
///
/// While experimenting with blocks, we found that we needed to have closure captures of variables
/// seen outside of the blocks, so that they blocks could be run in a way that was both thread-safe
/// and followed the restrictions for closures applied to iterators. The end result left us with
/// closure-captured single stack frames that blocks could see.
///
/// Blocks make up the only scope and stack definition abstraction in Nushell. As a result, we were
/// creating closure captures at any point we wanted to have a Block value we could safely evaluate
/// in any context. This meant that the parents were going largely unused, with captured variables
/// taking their place. The end result is this, where we no longer have separate frames, but instead
/// use the Stack as a way of representing the local and closure-captured state.
#[derive(Debug, Clone)]
pub struct Stack {
    /// Variables
    pub vars: Vec<VarInfo>,
    /// Environment variables arranged as a stack to be able to recover values from parent scopes
    pub env_vars: Vec<EnvVars>,
    /// Tells which environment variables from engine state are hidden, per overlay.
    pub env_hidden: HashMap<String, HashSet<String>>,
    /// List of active overlays
    pub active_overlays: Vec<String>,
    pub recursion_count: Box<u64>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            vars: vec![],
            env_vars: vec![],
            env_hidden: HashMap::new(),
            active_overlays: vec![DEFAULT_OVERLAY_NAME.to_string()],
            recursion_count: Box::new(0),
        }
    }

    pub fn with_env(
        &mut self,
        env_vars: &[EnvVars],
        env_hidden: &HashMap<String, HashSet<String>>,
    ) {
        // Do not clone the environment if it hasn't changed
        if self.env_vars.iter().any(|scope| !scope.is_empty()) {
            self.env_vars = env_vars.to_owned();
        }

        if !self.env_hidden.is_empty() {
            self.env_hidden = env_hidden.to_owned();
        }
    }

    pub fn get_var(&self, var_id: VarId, span: Span) -> Result<Value, ShellError> {
        for var_info in &self.vars {
            if var_id == var_info.var_id {
                return Ok(var_info.value.clone().with_span(span));
            }
        }

        Err(ShellError::VariableNotFoundAtRuntime { span })
    }

    pub fn get_var_with_origin(&self, var_id: VarId, span: Span) -> Result<Value, ShellError> {
        for var_info in &self.vars {
            if var_id == var_info.var_id {
                return Ok(var_info.value.clone());
            }
        }

        Err(ShellError::VariableNotFoundAtRuntime { span })
    }

    pub fn get_var_info(&self, var_id: VarId, span: Span) -> Result<VarInfo, ShellError> {
        for var_info in &self.vars {
            if var_id == var_info.var_id {
                return Ok(var_info.clone());
            }
        }

        Err(ShellError::VariableNotFoundAtRuntime { span })
    }

    pub fn add_var(&mut self, var_id: VarId, value: Value, expr: Option<&Expression>) {
        //self.vars.insert(var_id, value);
        for var_info in &mut self.vars {
            if var_info.var_id == var_id {
                *var_info = VarInfo::new(var_id, value, expr);
                return;
            }
        }
        self.vars.push(VarInfo::new(var_id, value, expr));
    }

    pub fn remove_var(&mut self, var_id: VarId) {
        for (idx, var_info) in self.vars.iter().enumerate() {
            if var_info.var_id == var_id {
                self.vars.remove(idx);
                return;
            }
        }
    }

    pub fn add_env_var(&mut self, var: String, value: Value) {
        if let Some(last_overlay) = self.active_overlays.last() {
            if let Some(env_hidden) = self.env_hidden.get_mut(last_overlay) {
                // if the env var was hidden, let's activate it again
                env_hidden.remove(&var);
            }

            if let Some(scope) = self.env_vars.last_mut() {
                if let Some(env_vars) = scope.get_mut(last_overlay) {
                    env_vars.insert(var, value);
                } else {
                    scope.insert(last_overlay.into(), [(var, value)].into_iter().collect());
                }
            } else {
                self.env_vars.push(
                    [(last_overlay.into(), [(var, value)].into_iter().collect())]
                        .into_iter()
                        .collect(),
                );
            }
        } else {
            // TODO: Remove panic
            panic!("internal error: no active overlay");
        }
    }

    pub fn last_overlay_name(&self) -> Result<String, ShellError> {
        self.active_overlays
            .last()
            .cloned()
            .ok_or_else(|| ShellError::NushellFailed {
                msg: "No active overlay".into(),
            })
    }

    pub fn captures_to_stack(&self, captures: &HashMap<VarId, VarInfo>) -> Stack {
        // FIXME: this is probably slow
        let mut env_vars = self.env_vars.clone();
        env_vars.push(HashMap::new());

        // FIXME make this more efficient
        let mut vars: Vec<VarInfo> = vec![];
        for var_info in captures.values() {
            vars.push(var_info.clone());
        }

        Stack {
            vars,
            env_vars,
            env_hidden: self.env_hidden.clone(),
            active_overlays: self.active_overlays.clone(),
            recursion_count: self.recursion_count.to_owned(),
        }
    }

    pub fn gather_captures(&self, engine_state: &EngineState, captures: &[VarId]) -> Stack {
        let mut vars = vec![];

        let fake_span = Span::new(0, 0);

        for capture in captures {
            // Note: this assumes we have calculated captures correctly and that commands
            // that take in a var decl will manually set this into scope when running the blocks
            if let Ok(var_info) = self.get_var_info(*capture, fake_span) {
                vars.push(var_info.clone());
            } else {
                let const_var = engine_state.get_var(*capture);
                if let Some(const_val) = &const_var.const_val {
                    vars.push(VarInfo::new_from_const(
                        *capture,
                        const_val.clone(),
                        const_var,
                    ));
                }
            }
        }

        let mut env_vars = self.env_vars.clone();
        env_vars.push(HashMap::new());

        Stack {
            vars,
            env_vars,
            env_hidden: self.env_hidden.clone(),
            active_overlays: self.active_overlays.clone(),
            recursion_count: self.recursion_count.to_owned(),
        }
    }

    /// Flatten the env var scope frames into one frame
    pub fn get_env_vars(&self, engine_state: &EngineState) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        for active_overlay in self.active_overlays.iter() {
            if let Some(env_vars) = engine_state.env_vars.get(active_overlay) {
                result.extend(
                    env_vars
                        .iter()
                        .filter(|(k, _)| {
                            if let Some(env_hidden) = self.env_hidden.get(active_overlay) {
                                !env_hidden.contains(*k)
                            } else {
                                // nothing has been hidden in this overlay
                                true
                            }
                        })
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect::<HashMap<String, Value>>(),
                );
            }
        }

        result.extend(self.get_stack_env_vars());

        result
    }

    /// Get flattened environment variables only from the stack
    pub fn get_stack_env_vars(&self) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        for scope in &self.env_vars {
            for active_overlay in self.active_overlays.iter() {
                if let Some(env_vars) = scope.get(active_overlay) {
                    result.extend(env_vars.clone());
                }
            }
        }

        result
    }

    /// Get flattened environment variables only from the stack and one overlay
    pub fn get_stack_overlay_env_vars(&self, overlay_name: &str) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        for scope in &self.env_vars {
            if let Some(active_overlay) = self.active_overlays.iter().find(|n| n == &overlay_name) {
                if let Some(env_vars) = scope.get(active_overlay) {
                    result.extend(env_vars.clone());
                }
            }
        }

        result
    }

    /// Same as get_env_vars, but returns only the names as a HashSet
    pub fn get_env_var_names(&self, engine_state: &EngineState) -> HashSet<String> {
        let mut result = HashSet::new();

        for active_overlay in self.active_overlays.iter() {
            if let Some(env_vars) = engine_state.env_vars.get(active_overlay) {
                result.extend(
                    env_vars
                        .keys()
                        .filter(|k| {
                            if let Some(env_hidden) = self.env_hidden.get(active_overlay) {
                                !env_hidden.contains(*k)
                            } else {
                                // nothing has been hidden in this overlay
                                true
                            }
                        })
                        .cloned()
                        .collect::<HashSet<String>>(),
                );
            }
        }

        for scope in &self.env_vars {
            for active_overlay in self.active_overlays.iter() {
                if let Some(env_vars) = scope.get(active_overlay) {
                    result.extend(env_vars.keys().cloned().collect::<HashSet<String>>());
                }
            }
        }

        result
    }

    pub fn get_env_var(&self, engine_state: &EngineState, name: &str) -> Option<Value> {
        for scope in self.env_vars.iter().rev() {
            for active_overlay in self.active_overlays.iter().rev() {
                if let Some(env_vars) = scope.get(active_overlay) {
                    if let Some(v) = env_vars.get(name) {
                        return Some(v.clone());
                    }
                }
            }
        }

        for active_overlay in self.active_overlays.iter().rev() {
            let is_hidden = if let Some(env_hidden) = self.env_hidden.get(active_overlay) {
                env_hidden.contains(name)
            } else {
                false
            };

            if !is_hidden {
                if let Some(env_vars) = engine_state.env_vars.get(active_overlay) {
                    if let Some(v) = env_vars.get(name) {
                        return Some(v.clone());
                    }
                }
            }
        }

        None
    }

    pub fn has_env_var(&self, engine_state: &EngineState, name: &str) -> bool {
        for scope in self.env_vars.iter().rev() {
            for active_overlay in self.active_overlays.iter().rev() {
                if let Some(env_vars) = scope.get(active_overlay) {
                    if env_vars.contains_key(name) {
                        return true;
                    }
                }
            }
        }

        for active_overlay in self.active_overlays.iter().rev() {
            let is_hidden = if let Some(env_hidden) = self.env_hidden.get(active_overlay) {
                env_hidden.contains(name)
            } else {
                false
            };

            if !is_hidden {
                if let Some(env_vars) = engine_state.env_vars.get(active_overlay) {
                    if env_vars.contains_key(name) {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn remove_env_var(&mut self, engine_state: &EngineState, name: &str) -> bool {
        for scope in self.env_vars.iter_mut().rev() {
            for active_overlay in self.active_overlays.iter().rev() {
                if let Some(env_vars) = scope.get_mut(active_overlay) {
                    if env_vars.remove(name).is_some() {
                        return true;
                    }
                }
            }
        }

        for active_overlay in self.active_overlays.iter().rev() {
            if let Some(env_vars) = engine_state.env_vars.get(active_overlay) {
                if env_vars.get(name).is_some() {
                    if let Some(env_hidden) = self.env_hidden.get_mut(active_overlay) {
                        env_hidden.insert(name.into());
                    } else {
                        self.env_hidden
                            .insert(active_overlay.into(), [name.into()].into_iter().collect());
                    }

                    return true;
                }
            }
        }

        false
    }

    pub fn has_env_overlay(&self, name: &str, engine_state: &EngineState) -> bool {
        for scope in self.env_vars.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }

        engine_state.env_vars.contains_key(name)
    }

    pub fn is_overlay_active(&self, name: &str) -> bool {
        self.active_overlays.iter().any(|n| n == name)
    }

    pub fn add_overlay(&mut self, name: String) {
        self.active_overlays.retain(|o| o != &name);
        self.active_overlays.push(name);
    }

    pub fn remove_overlay(&mut self, name: &str) {
        self.active_overlays.retain(|o| o != name);
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}
