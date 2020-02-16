use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnSignature(pub String, pub usize, pub usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Frame {
    varcount: usize,
    local_vars: HashMap<String, usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Env {
    lbl_counter: usize,
    frames: Vec<Frame>,
    functions: HashMap<String, FnSignature>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            varcount: 0,
            local_vars: HashMap::new(),
        }
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            lbl_counter: 0,
            frames: vec![],
            functions: HashMap::new(),
        }
    }

    pub fn new_label(&mut self) -> String {
        let l = format!("label_{}", self.lbl_counter);
        self.lbl_counter += 1;
        l
    }

    pub fn new_local(&mut self, name: String) {
        let mut frame = self.top_frame_or_new();
        frame.local_vars.insert(name, frame.varcount);
        frame.varcount += 1;
    }

    pub fn new_function(&mut self, name: String, sig: FnSignature) {
        self.functions.insert(name, sig);
    }

    pub fn new_tmp(&mut self) -> String {
        let l = format!("reg_{}", self.lbl_counter);
        self.lbl_counter += 1;
        l
    }

    pub fn lookup_top(&self, x: &str) -> Option<usize> {
        self.top_frame().and_then(|f| f.local_vars.get(x).cloned())
    }

    pub fn lookup_full(&self, x: &str) -> Option<usize> {
        self.frames
            .iter()
            .rev()
            .map(|f| f.local_vars.get(x))
            .filter(Option::is_some)
            .next()
            .flatten()
            .cloned()
    }

    pub fn lookup_function(&self, f: &str) -> Option<FnSignature> {
        self.functions.get(f).cloned()
    }

    pub fn push_frame(&mut self) {
        self.frames.push(Frame::new())
    }

    pub fn top_frame(&self) -> Option<&Frame> {
        self.frames.last()
    }

    pub fn top_frame_mut(&mut self) -> Option<&mut Frame> {
        self.frames.last_mut()
    }

    pub fn top_frame_or_new(&mut self) -> &mut Frame {
        if self.frames.len() == 0 {
            self.push_frame();
        }
        self.top_frame_mut().unwrap()
    }

    pub fn pop_frame(&mut self) -> Option<Frame> {
        self.frames.pop()
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
