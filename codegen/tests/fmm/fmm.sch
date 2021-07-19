traversal fuse {
  case CVirtualRoot {
    eval root.parentPotential;
    recur root;
  }
  case CVertex {
    eval self.interactionList;
    eval self.x1;
    eval self.y1;
    eval point.potential;
    eval point.finalPotential;
    eval point.basePotential;
    iterate[left] c1 {
      eval c1.parentPotential;
      recur c1;
    }
    iterate[left] c2 {
      eval c2.parentPotential;
      recur c2;
    }
    iterate[left] c3 {
      eval c3.parentPotential;
      recur c3;
    }
    iterate[left] c4 {
      eval c4.parentPotential;
      recur c4;
    }
    recur point;
  }
  case PointList {
    iterate[left] next {
      eval next.potential;
      eval next.finalPotential;
      eval next.basePotential;
      recur next;
    }
  }
}