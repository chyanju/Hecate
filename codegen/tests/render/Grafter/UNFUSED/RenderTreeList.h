#ifndef RENDER_TREE_LIST
#define RENDER_TREE_LIST
#define __tree_structure__ __attribute__((annotate("tf_tree")))
#define __tree_child__ __attribute__((annotate("tf_child")))
#define __tree_traversal__ __attribute__((annotate("tf_fuse")))
#define __abstract_access__(AccessList)                                        \
  __attribute__((annotate("tf_strict_access" #AccessList)))

#ifdef COUNT_VISITS
int  _VISIT_COUNTER =0;
#endif

#include "RenderTree.h"
#include "Types.h"
#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
using namespace std;


class LDocument;
class LPage;
class LHorizontalContainer;

class LVerticalContainer;

class LElement;

class __tree_structure__ LDocument : public Node {
public:
  FontInfo FontStyle;

  __tree_child__ vector<LPage> PageList;
  __tree_traversal__ void resolveFlexWidths();
  __tree_traversal__ void resolveRelativeWidths();
  __tree_traversal__ void computeHeights();
  __tree_traversal__ void setFont();
  __tree_traversal__ void setPositions();
  int computeTreeSize() ;
  void print() override;
  
  // Hecate
  void eval();
  virtual ostream& format(ostream & out) const override;
};

class __tree_structure__ LPage : public Node, public Data {
public:
  __tree_child__ vector<HorizontalContainer> HorizList;
  __tree_traversal__ void resolveRelativeWidths();
  __tree_traversal__ void resolveFlexWidths();
  __tree_traversal__ void computeHeights();
  __tree_traversal__ void setFont(FontInfo FontStyle);
  __tree_traversal__ void setPositions();
  void print() override;
  int computeTreeSize() ;
  
  // Hecate
  void eval(PageListNode *node, FontInfo ParentFontStyle);
  virtual ostream& format(ostream & out) const override;

};


class __tree_structure__ HorizontalContainer : public Node, public Data {
public:
  __tree_child__ vector<LElement> ElementList;
  __tree_traversal__ void resolveFlexWidths();
  __tree_traversal__ void resolveRelativeWidths(int PWidth);
  __tree_traversal__ void computeHeights();
  __tree_traversal__ void setFont(FontInfo FontStyle);
  __tree_traversal__ void setPositions(int CurrX, int CurrY);
  void print() override;
  int computeTreeSize();
  
  // Hecate
  void eval(HorizontalContainerListNode *node, FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth);
  virtual ostream& format(ostream & out) const override;
};

//-------------- Elements ---------------
class __tree_structure__ LElement : public Node, public Data {
public:
  virtual __tree_traversal__ void resolveFlexWidths();
  virtual __tree_traversal__ void resolveRelativeWidths(int PWidth);
  virtual __tree_traversal__ void computeHeights(){};
  virtual __tree_traversal__ void setFont(FontInfo FontStyle);
  virtual __tree_traversal__ void setPositions(int CurrX, int CurrY);
  virtual int computeTreeSize() =0;
  
  // Hecate
  virtual void eval(ElementListNode *node, FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth);
  virtual int evalHeight();
  virtual ostream& format(ostream & out) const override {return out;};
};

class __tree_structure__ TextBox : public Element {

public:
  String ContentText;
  __tree_traversal__ void computeHeights() override;
  void print() override;
  int computeTreeSize() override;
  virtual ostream& format(ostream & out) const override;
};


class __tree_structure__ List : public Element {
public:
  ListItems Items;
  int ItemMargin;
  __tree_traversal__ void computeHeights() override;
  void print() override;
  int computeTreeSize() override;
  virtual ostream& format(ostream & out) const override;
};


class __tree_structure__ Image : public Element {
public:
  string path_to_image;
  float ImageOriginalWidth;
  float ImageOriginalHeight;
  String ImageURL;
  __tree_traversal__ void computeHeights() override;
  void print() override;
  int computeTreeSize() override;
  virtual ostream& format(ostream & out) const override;
};


class __tree_structure__ VerticalContainer : public Element {
public:
  __tree_child__ HorizontalContainerListNode *HorizList;
  __tree_traversal__ void resolveFlexWidths() override;
  __tree_traversal__ void resolveRelativeWidths(int PWidth) override;
  __tree_traversal__ void computeHeights() override;
  __tree_traversal__ void setFont(FontInfo FontStyle) override;
  __tree_traversal__ void setPositions(int CurrX, int CurrY) override;
  void print() override;
  int computeTreeSize() override;
  
  // Hecate
  virtual void eval(ElementListNode *node, FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) override;
  virtual int evalHeight() override;
  virtual ostream& format(ostream & out) const override;
};

//--------------------------------------------
#endif
