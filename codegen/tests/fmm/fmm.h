Standard ML of New Jersey (64-bit) v110.99 [built: Thu Dec 24 11:47:23 2020]
[scanning sources.cm]
[attempting to load plugin $/lex-ext.cm]
[library $/lex-ext.cm is stable]
[library $smlnj/cm/tools.cm is stable]
[library $smlnj/internal/cm-lib.cm is stable]
[library $SMLNJ-BASIS/basis.cm is stable]
[library $SMLNJ-BASIS/(basis.cm):basis-common.cm is stable]
[plugin $/lex-ext.cm loaded successfully]
[attempting to load plugin $/mllex-tool.cm]
[library $/mllex-tool.cm is stable]
[plugin $/mllex-tool.cm loaded successfully]
[attempting to load plugin $/grm-ext.cm]
[library $/grm-ext.cm is stable]
[plugin $/grm-ext.cm loaded successfully]
[attempting to load plugin $/mlyacc-tool.cm]
[library $/mlyacc-tool.cm is stable]
[plugin $/mlyacc-tool.cm loaded successfully]
[library $/ml-yacc-lib.cm is stable]
[library $SMLNJ-LIB/Util/smlnj-lib.cm is stable]
[library $SMLNJ-ML-YACC-LIB/ml-yacc-lib.cm is stable]
[loading (sources.cm):string-hash-table.sml]
[loading (sources.cm):FrontEnd/Spec/ctype.sml]
[loading (sources.cm):FrontEnd/Spec/path.sml]
[loading (sources.cm):prelude.sml]
[loading (sources.cm):FrontEnd/Spec/expr.sml]
[loading (sources.cm):FrontEnd/Spec/class.sml]
[loading (sources.cm):FrontEnd/Spec/schedule.sml]
[loading (sources.cm):FrontEnd/Spec/interface.sml]
[loading (sources.cm):FrontEnd/Spec/ast.sml]
[loading (sources.cm):FrontEnd/Parse/hecate.grm.sig]
[loading (sources.cm):FrontEnd/Parse/errormsg.sml]
[loading (sources.cm):FrontEnd/Parse/hecate.lex.sml]
[loading (sources.cm):FrontEnd/Parse/hecate-schedule.grm.sig]
[loading (sources.cm):CodeGen/resolve-eval.sml]
[loading (sources.cm):CodeGen/sort-eval.sml]
[loading (sources.cm):CodeGen/code-gen.sml]
[loading (sources.cm):FrontEnd/Spec/traversal.sml]
[loading (sources.cm):table.sig]
[loading (sources.cm):table.sml]
[loading (sources.cm):symbol.sml]
[loading (sources.cm):CodeGen/optimize.sml]
[loading (sources.cm):FrontEnd/Parse/hecate.grm.sml]
[loading (sources.cm):FrontEnd/Parse/hecate-schedule.grm.sml]
[loading (sources.cm):FrontEnd/Parse/parse.sml]
[loading (sources.cm):main.sml]
Heap was already up-to-date.
tests/fmm/fmm.grammar-aug
tests/fmm/fmm.sch

#include <vector>

using namespace std;

#define NULL_VAL 9999

class Union {
public:
  virtual void eval() = 0;
};

class VirtualRoot : public Union {
public:
};

class Point : public Union {
public:
  int mass;           // input
  int coordX;         // input
  int coordY;         // input
  int potential;      // output
  int finalPotential; // output
  int basePotential;  // output
};

class Vertex : public Union {
public:
  int box_startX;                 // input
  int box_startY;                 // input
  int box_endX;                   // input
  int box_endY;                   // input
  int parentPotential;            // output
  vector<Vertex> interactionList; // output
  int x1;                         // output
  int y1;                         // output
};

class CVertex : Vertex {
public:
  Point *point;
  Vertex *c2;
  Vertex *c1;
  Vertex *c4;
  Vertex *c3;

  void eval() override {
    this->interactionList = getInteractionList();
    this->x1 = ((this->box_startX) + (this->box_endX)) / (2);
    this->y1 = ((this->box_startY) + (this->box_endY)) / (2);
    this->point->potential =
        (0) + (getPotential(this->x1, this->y1, this->point->mass,
                            this->interactionList));
    this->point->finalPotential =
        (this->point->potential) + (this->parentPotential);
    this->point->basePotential = this->point->finalPotential;
    if (this->c1 != NULL) {
      this->c1->parentPotential = this->point->finalPotential;
      if (this->c1 != NULL) {
        this->c1->eval();
      }
    }
    if (this->c2 != NULL) {
      this->c2->parentPotential = this->point->finalPotential;
      if (this->c2 != NULL) {
        this->c2->eval();
      }
    }
    if (this->c3 != NULL) {
      this->c3->parentPotential = this->point->finalPotential;
      if (this->c3 != NULL) {
        this->c3->eval();
      }
    }
    if (this->c4 != NULL) {
      this->c4->parentPotential = this->point->finalPotential;
      if (this->c4 != NULL) {
        this->c4->eval();
      }
    }
    this->point->eval();
  }
};

class CVirtualRoot : VirtualRoot {
public:
  Vertex *root;

  void eval() override {
    this->root->parentPotential = 0;
    this->root->eval();
  }
};

class PointList : Point {
public:
  Point *next;

  void eval() override {
    if (this->next != NULL) {
      this->next->potential = 0;
      this->next->finalPotential =
          (this->next->potential) + (this->basePotential);
      this->next->basePotential = this->basePotential;
      if (this->next != NULL) {
        this->next->eval();
      }
    }
  }
};
