traversal fuse {
  case CVirtualRoot {
    eval root.final0_args;
    recur root;
  }
  case CNode {
    iterate[left] left {
      eval left.final0_args;
      recur left;
    }
    iterate[left] right {
      eval right.final0_args;
      recur right;
    }
    eval self.has_left;
    eval self.id;
    eval self.has_right;
    eval self.is_leaf;
    eval self.final0_coeff;
  }
}