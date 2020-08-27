#include <assert.h>
#include <chrono>
#include <stddef.h>
#include <stdio.h>

#define __tree_structure__ __attribute__((annotate("tf_tree")))
#define __tree_child__ __attribute__((annotate("tf_child")))
#define __tree_traversal__ __attribute__((annotate("tf_fuse")))

enum NodeType { VAL_NODE, NULL_NODE };

class __tree_structure__ Node {
public:
  NodeType Type;
  __tree_traversal__ virtual void f1() {}
  __tree_traversal__ virtual void f2() {}
  virtual void __virtualStub0(unsigned int truncate_flags);
  virtual void __virtualStub1(unsigned int truncate_flags);
};

class __tree_structure__ NullNode : public Node {
  __tree_traversal__ void f1() { return; }
  __tree_traversal__ void f2() { return; }
  void __virtualStub0(unsigned int truncate_flags) override;
  void __virtualStub1(unsigned int truncate_flags) override;
};

class __tree_structure__ ValueNode : public Node {
public:
  int v, x, y;
  __tree_child__ Node *Left, *Right;
  __tree_traversal__ void f1() {
    bool is_leaf;
    is_leaf = (Left->Type == NULL_NODE) && (Right->Type == NULL_NODE);
    Left->f1();
    Right->f1();
    if (is_leaf) {
      v = 1;
    } else {
      v = static_cast<ValueNode *>(Left)->v +
          static_cast<ValueNode *>(Right)->v;
    }
    return;
  }
  __tree_traversal__ void f2() {
    bool not_is_leaf;
    not_is_leaf = (Left->Type == VAL_NODE) || (Right->Type == VAL_NODE);
    if (not_is_leaf) {
      x = v;
      static_cast<ValueNode *>(Left)->x = x + 2;
    }
    y = v * 2;
    Left->f2();
    Right->f2();
  }
  void __virtualStub0(unsigned int truncate_flags) override;
  void __virtualStub1(unsigned int truncate_flags) override;
};

Node *createTree() {
  Node *Root = new ValueNode();
  ValueNode *const _Root = static_cast<ValueNode *>(Root);
  _Root->v = 5;
  _Root->Type = VAL_NODE;

  Node *N3 = new ValueNode();
  ValueNode *const _N3 = static_cast<ValueNode *>(N3);
  _N3->v = 3;
  _N3->Type = VAL_NODE;

  Node *N2 = new ValueNode();
  ValueNode *const _N2 = static_cast<ValueNode *>(N2);
  _N2->v = 2;
  _N2->Type = VAL_NODE;

  Node *N4 = new ValueNode();
  ValueNode *const _N4 = static_cast<ValueNode *>(N4);
  _N4->v = 4;
  _N4->Type = VAL_NODE;

  Node *N2_L = new NullNode();
  N2_L->Type = NULL_NODE;
  Node *N2_R = new NullNode();
  N2_R->Type = NULL_NODE;

  Node *N4_L = new NullNode();
  N4_L->Type = NULL_NODE;
  Node *N4_R = new NullNode();
  N4_R->Type = NULL_NODE;

  _N3->Left = N2;
  _N3->Right = N4;
  _N2->Left = N2_L;
  _N2->Right = N2_R;
  _N4->Left = N4_L;
  _N4->Right = N4_R;

  Node *N7 = new ValueNode();
  ValueNode *const _N7 = static_cast<ValueNode *>(N7);
  _N7->v = 7;
  _N7->Type = VAL_NODE;

  Node *N6 = new ValueNode();
  ValueNode *const _N6 = static_cast<ValueNode *>(N6);
  _N6->v = 6;
  _N6->Type = VAL_NODE;

  Node *N8 = new ValueNode();
  ValueNode *const _N8 = static_cast<ValueNode *>(N8);
  _N8->v = 8;
  _N8->Type = VAL_NODE;

  Node *N6_L = new NullNode();
  N6_L->Type = NULL_NODE;
  Node *N6_R = new NullNode();
  N6_R->Type = NULL_NODE;

  Node *N8_L = new NullNode();
  N8_L->Type = NULL_NODE;
  Node *N8_R = new NullNode();
  N8_R->Type = NULL_NODE;

  _N7->Left = N6;
  _N7->Right = N8;
  _N6->Left = N6_L;
  _N6->Right = N6_R;
  _N8->Left = N8_L;
  _N8->Right = N8_R;

  _Root->Left = N3;
  _Root->Right = N7;

  return Root;
}

void _fuse__F5F6(ValueNode *_r, unsigned int truncate_flags);
void _fuse__F1F2(Node *_r, unsigned int truncate_flags);
void _fuse__F3F4(NullNode *_r, unsigned int truncate_flags);
void _fuse__F5F6(ValueNode *_r, unsigned int truncate_flags) {

#ifdef COUNT_VISITS
  _VISIT_COUNTER++;
#endif
  ValueNode *_r_f0 = (ValueNode *)(_r);
  ValueNode *_r_f1 = (ValueNode *)(_r);
  _Bool _f0_is_leaf;
  if (truncate_flags & 0b1) {
    _f0_is_leaf =
        (_r_f0->Left->Type == NULL_NODE) && (_r_f0->Right->Type == NULL_NODE);
  }
_label_B1F0_Exit:
  if ((truncate_flags & 0b1)) /*call*/ {
    _r_f0->Left->f1();
  }
  if ((truncate_flags & 0b11)) /*call*/ {
    unsigned int AdjustedTruncateFlags = 0;
    AdjustedTruncateFlags <<= 1;
    AdjustedTruncateFlags |= (0b01 & (truncate_flags >> 1));
    AdjustedTruncateFlags <<= 1;
    AdjustedTruncateFlags |= (0b01 & (truncate_flags >> 0));
    _r_f0->Right->__virtualStub0(AdjustedTruncateFlags);
  }
  if (truncate_flags & 0b1) {
    if (_f0_is_leaf) {
      _r_f0->v = 1;
    } else {
      _r_f0->v = static_cast<class ValueNode *>(_r_f0->Left)->v +
                 static_cast<class ValueNode *>(_r_f0->Right)->v;
    }
    truncate_flags &= 0b11111111110;
    goto _label_B3F0_Exit;
  }
_label_B3F0_Exit:
  _Bool _f1_not_is_leaf;
  if (truncate_flags & 0b10) {
    _r_f1->y = _r_f1->v * 2;
    _f1_not_is_leaf =
        (_r_f1->Left->Type == VAL_NODE) || (_r_f1->Right->Type == VAL_NODE);
    if (_f1_not_is_leaf) {
      _r_f1->x = _r_f1->v;
      static_cast<class ValueNode *>(_r_f1->Left)->x = _r_f1->x + 2;
    }
  }
_label_B3F1_Exit:
  if ((truncate_flags & 0b10)) /*call*/ {
    _r_f1->Left->f2();
  }
  return;
};
void _fuse__F1F2(Node *_r, unsigned int truncate_flags) {

#ifdef COUNT_VISITS
  _VISIT_COUNTER++;
#endif
  Node *_r_f0 = (Node *)(_r);
  Node *_r_f1 = (Node *)(_r);
  return;
};
void _fuse__F3F4(NullNode *_r, unsigned int truncate_flags) {

#ifdef COUNT_VISITS
  _VISIT_COUNTER++;
#endif
  NullNode *_r_f0 = (NullNode *)(_r);
  NullNode *_r_f1 = (NullNode *)(_r);
  if (truncate_flags & 0b1) {
    truncate_flags &= 0b11111111110;
    goto _label_B1F0_Exit;
  }
_label_B1F0_Exit:
  if (truncate_flags & 0b10) {
    truncate_flags &= 0b11111111101;
    goto _label_B1F1_Exit;
  }
_label_B1F1_Exit:
  return;
};
void Node::__virtualStub0(unsigned int truncate_flags) {
  _fuse__F1F2(this, truncate_flags);
}
void NullNode::__virtualStub0(unsigned int truncate_flags) {
  _fuse__F3F4(this, truncate_flags);
}
void ValueNode::__virtualStub0(unsigned int truncate_flags) {
  _fuse__F5F6(this, truncate_flags);
}
void Node::__virtualStub1(unsigned int truncate_flags) {
  _fuse__F1F2(this, truncate_flags);
}
void NullNode::__virtualStub1(unsigned int truncate_flags) {
  _fuse__F3F4(this, truncate_flags);
}
void ValueNode::__virtualStub1(unsigned int truncate_flags) {
  _fuse__F5F6(this, truncate_flags);
}
int main() {
  Node *Root = createTree();
  // here starts the part that requires fusion
  // Root -> f1();
  // Root -> f2();

  // added by fuse transformer
  Root->__virtualStub1(0b11);
}
