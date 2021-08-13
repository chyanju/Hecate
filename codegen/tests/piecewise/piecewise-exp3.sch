traversal fuse {
  case CVirtualRoot {
    eval root.final0_arg;
    eval root.final1_arg;
    eval root.final3_arg;
    recur root;
  }
  case CNode {
    iterate[left] left {
      eval left.final1_arg;
      eval left.final3_arg;
      eval left.final0_arg;
      recur left;
    }
    iterate[left] right {
      eval right.final1_arg;
      eval right.final3_arg;
      eval right.final0_arg;
      recur right;
    }
    eval self.has_right;
    eval self.has_left;
    eval self.id;
    eval self.is_leaf;
    eval self.final0_coeff;
    eval self.final1_coeff;
    eval self.final2_coeff;
    eval self.final3_project_val;
  }
}