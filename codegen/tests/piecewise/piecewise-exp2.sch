traversal fuse {
  case CVirtualRoot {
    eval root.final1_arg_x;
    recur root;
  }
  case CNode {
    iterate[left] left {
      eval left.final1_arg_x;
      recur left;
    }
    iterate[left] right {
      eval right.final1_arg_x;
      recur right;
    }
    eval self.has_left;
    eval self.final0_project_val;
    eval self.id;
    eval self.has_right;
    eval self.is_leaf;
    eval self.final0_coeff;
    eval self.final1_project_val;
    eval self.final1_coeff;
  }
}