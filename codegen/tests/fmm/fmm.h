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
