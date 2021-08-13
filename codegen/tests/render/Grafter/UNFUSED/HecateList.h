#ifndef HECATE_LIST
#define HECATE_LIST

#include "RenderTree.h"


/*************************************************
 *               Populate Lists
 *************************************************/
void Document::populate() {
  PageListNode* curr = PageList;
  while (curr) {
    Page *p = curr->Content;
    p->populate();
    Pages.push_back(p);
    curr = curr->_next();
  }
}

void Page::populate() {
  HorizontalContainerListNode* curr = HorizList;
  while (curr) {
    HorizontalContainer *x = curr->Content;
    x->populate();
    Horizontals.push_back(x);
    curr = curr->_next();
  }
}

void HorizontalContainer::populate() {
  ElementListNode* curr = ElementsList;
  while (curr) {
    Element *x = curr->Content;
    x->populate();
    Elements.push_back(x);
    curr = curr->_next();
  }
}

void VerticalContainer::populate() {
  HorizontalContainerListNode* curr = HorizList;
  while (curr) {
    HorizontalContainer *x = curr->Content;
    x->populate();
    Horizontals.push_back(x);
    curr = curr->_next();
  }
}


/*************************************************
 *              _resolveFlexWidths
 *************************************************/
void Document::_resolveFlexWidths() {
  // Always enable parallelism at the top level
  #pragma omp parallel for
  for (auto p : Pages) {
    p->_resolveFlexWidths();
  }
}

void Page::_resolveFlexWidths() {
#ifdef PAR
  #pragma omp parallel for
  for (auto x : Horizontals) {
    x->_resolveFlexWidths();
  }
  
  auto x = Horizontals.begin();
  int w = (*x)->Width;
  ++x;
  for (; x != Horizontals.end(); ++x) {
    w = max(w, (*x)->Width);
  }
#else
  auto x = Horizontals.begin();
  int w = (*x)->_resolveFlexWidths();
  ++x;
  for (; x != Horizontals.end(); ++x) {
    w = max(w, (*x)->_resolveFlexWidths());
  }
#endif /* PAR */

  if (WMode == FLEX) {
    Width = w;
  }
  HorizList->MaxWidth = w;
}

int HorizontalContainer::_resolveFlexWidths() {
#ifdef PAR
  #pragma omp parallel for
  for (auto x : Elements) {
    x->_resolveFlexWidths();
  }
  auto x = Elements.begin();
  int w = (*x)->Width;
  ++x;
  for (; x != Elements.end(); ++x) {
    w += (*x)->Width;
  }
#else
  auto e = Elements.begin();
  int w = (*e)->_resolveFlexWidths();
  ++e;
  for (; e != Elements.end(); ++e) {
    w += (*e)->_resolveFlexWidths();
  }
#endif /* PAR */

  if (WMode == FLEX) {
    Width = w;
  }
  ElementsList->AccumulatedWidth = w;
  return Width;
}

int Element::_resolveFlexWidths() {
  if (WMode == FLEX) {
    printf("Generic element should not have FLEX width");
  }
  return Width;
}

int VerticalContainer::_resolveFlexWidths() {
#ifdef PAR
  #pragma omp parallel for
  for (auto x : Horizontals) {
    x->_resolveFlexWidths();
  }
  
  auto x = Horizontals.begin();
  int w = (*x)->Width;
  ++x;
  for (; x != Horizontals.end(); ++x) {
    w = max(w, (*x)->Width);
  }
#else
  auto h = Horizontals.begin();
  int w = (*h)->_resolveFlexWidths();
  ++h;
  for (auto h = Horizontals.begin(); h != Horizontals.end(); ++h) {
    w = max(w, (*h)->_resolveFlexWidths());
  }
#endif
  
  if (WMode == FLEX) {
    Width = w;
  }
  HorizList->MaxWidth = w;
  return Width;
}


/*************************************************
 *                     _eval
 *************************************************/
void Document::_eval() {
  // iterate[left] PageList {
  for (auto p : Pages) {
    // eval PageList.PosX;
    p->PosX = 0;
    // eval PageList.PosY;
    p->PosY = 0;
    // eval PageList.ParentFontStyle;
    // recur PageList;
    p->_eval(this->FontStyle);
  // }
  }
}

void Page::_eval(FontInfo ParentFontStyle) {
  Page *self = this;
  // eval self.FontStyle1;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  // eval self.WidthFlex;
  int WidthFlex = self->HorizList->MaxWidth;
  // eval self.WidthRel; // not used
  // eval self.CurrX;
  int CurrX = self->PosX;
  int CurrY = self->PosY;
  // eval self.Width;
  switch (self->WMode) {
    case FLEX:
      self->Width = WidthFlex;
      break;
    case REL:
      printf("Page cannot have FLEX width\n");
      break;
    case ABS: default: break;
  }
  // iterate[left] HorizList {
  for (auto h: self->Horizontals) {
    // eval HorizList.PosX;
    // eval HorizList.ParentFontStyle;
    // eval HorizList.PosY;
    // eval HorizList.PWidth;
    // recur HorizList;
    h->PosX = CurrX;
    h->PosY = CurrY;
    h->_eval(self->FontStyle, self->Width);
    // eval self.CurrY;
    CurrY += h->Height;
    // eval self.AggregatedHeight;
  // }
  }
  // eval self.Height;
  self->Height = CurrY;
}

void HorizontalContainer::_eval(FontInfo ParentFontStyle, int PWidth) {
  HorizontalContainer *self = this;
  // eval self.WidthFlex;
  int WidthFlex = self->ElementsList->AccumulatedWidth;
  // eval self.WidthRel;
  int WidthRel = self->RelWidth * PWidth;
  // eval self.Width;
  switch (self->WMode) {
    case FLEX:
      self->Width = WidthFlex;
      break;
    case REL:
      self->Width = WidthRel;
      break;
    case ABS: default: break;
  }
  // eval self.FontStyle1;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  // eval self.CurrY;
  int CurrX = self->PosX;
  int CurrY = self->PosY;
  int MaxHeight = 0;
  for (auto e: Elements) {
  // iterate[left] ElementList {
    // eval ElementList.PosY;
    e->PosY = CurrY;
    // eval ElementList.PosX;
    e->PosX = CurrX;
    // eval ElementList.PWidth;
    // eval ElementList.ParentFontStyle;
    // recur ElementList;
    e->_eval(self->FontStyle, self->Width);
    // eval self.MaxHeight;
    MaxHeight = max(MaxHeight, e->Height);
    // eval self.CurrX;
    CurrX += e->Width;
  // }
  }
  // eval self.Height;
  self->Height = MaxHeight;
}

void Element::_eval(FontInfo ParentFontStyle, int PWidth) {
  Element *self = this;
  // eval self.WidthFlex; // not used
  // eval self.CurrY; // not used
  // eval self.WidthRel;
  int WidthRel = self->RelWidth * PWidth;
  // eval self.CurrX; // not used
  // eval self.Width;
  switch (self->WMode) {
    case FLEX:
      printf("Generic element should not have FLEX width");
      break;
    case REL:
      self->Width = WidthRel;
      break;
    case ABS: default: break;
  }
  // eval self.AggregatedHeight; // not used
  // eval self.FontStyle1;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  // eval self.Height;
  self->Height = evalHeight();
}

void VerticalContainer::_eval(FontInfo ParentFontStyle, int PWidth) {
  VerticalContainer *self = this;
  // eval self.WidthRel;
  int WidthRel = self->RelWidth * PWidth;
  // eval self.WidthFlex;
  int WidthFlex = self->HorizList->MaxWidth;
  // eval self.CurrX;
  int CurrX = self->PosX;
  int CurrY = self->PosY;
  // eval self.FontStyle1;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  // eval self.Width;
  switch (self->WMode) {
    case FLEX:
      self->Width = WidthFlex;
      break;
    case REL:
      self->Width = WidthRel;
      break;
    case ABS: default: break;
  }
  // iterate[left] HorizList {
  for (auto h: Horizontals) {
    // eval HorizList.ParentFontStyle;
    // eval HorizList.PWidth;
    // eval HorizList.PosY;
    h->PosY = CurrY;
    // eval HorizList.PosX;
    h->PosX = CurrX;
    // recur HorizList;
    h->_eval(self->FontStyle, self->Width);
    // eval self.CurrY;
    CurrY += h->Height;
  // }
  }
  // eval self.Height;
  self->Height = CurrY;
}


#endif /* HECATE_LIST */