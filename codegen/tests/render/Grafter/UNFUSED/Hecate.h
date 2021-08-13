#include "RenderTree.h"

#include <iostream>
using namespace std;

void Document::eval() {
  // eval PageList.ParentFontStyle = self.FontStyle
  // recur PageList;
  PageList->eval(this->FontStyle);
}

/*************************************************
 *                   Pages
 *************************************************/
void PageListNode::eval(FontInfo ParentFontStyle) {
}

void PageListEnd::eval(FontInfo ParentFontStyle) {
  Page *self = Content;
  PageListNode *node = this;
  // eval self.PosX;
  self->PosX = 0;
  // eval self.PosY;
  self->PosY = 0;
  // eval self.FontStyle1
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  
  // eval self.WidthFlex;
  int WidthFlex = self->HorizList->MaxWidth;
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
  //   eval HorizList.CurrX;
  //   eval HorizList.ParentFontStyle;
  //   eval HorizList.CurrY;
  //   eval HorizList.PWidth;
  //   recur HorizList;
  // }
  self->HorizList->eval(self->FontStyle, self->PosX, self->PosY, self->Width);
  // eval self.Height; (adjusted)
  self->Height = self->HorizList->AggregatedHeight;

  // iterate[left] Next {
  //   eval Next.ParentFontStyle;
  //   recur Next;
  // }
}

void PageListInner::eval(FontInfo ParentFontStyle) {
  Page *self = Content;
  PageListNode *node = this;
  // eval self.PosX;
  self->PosX = 0;
  // eval self.PosY;
  self->PosY = 0;
  // eval self.FontStyle1
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  
  // eval self.WidthFlex;
  int WidthFlex = self->HorizList->MaxWidth;
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
  //   eval HorizList.CurrX;
  //   eval HorizList.ParentFontStyle;
  //   eval HorizList.CurrY;
  //   eval HorizList.PWidth;
  //   recur HorizList;
  // }
  self->HorizList->eval(self->FontStyle, self->PosX, self->PosY, self->Width);
  // eval self.Height; (adjusted)
  self->Height = self->HorizList->AggregatedHeight;

  // iterate[left] Next {
  //   eval Next.ParentFontStyle;
  //   recur Next;
  // }
  NextPage->eval(ParentFontStyle);
}





/*************************************************
 *             Horizontal Containers
 *************************************************/
void HorizontalContainerListNode::eval(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
}

void HorizontalContainerListEnd::eval(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  HorizontalContainer *self = Content;
  HorizontalContainerListNode *node = this;
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
  
  // eval self.PosY;
  self->PosY = CurrY;
  // eval self.PosX;
  self->PosX = CurrX;
  
  // iterate[left] ElementList {
  //   eval ElementList.PWidth;
  //   eval ElementList.ParentFontStyle;
  //   eval ElementList.CurrX;
  //   eval ElementList.CurrY;
  //   recur ElementList;
  // }
  self->ElementsList->eval(self->FontStyle, CurrX, CurrY, self->Width);
    
  // eval self.Height;
  self->Height = self->ElementsList->MaxHeight;
  
  // iterate[left] Next {
  //   eval Next.CurrY;
  //   eval Next.CurrX;
  //   eval Next.ParentFontStyle;
  //   eval Next.PWidth;
  //   recur Next;
  // }
  // no next
  
  // eval self.AggregatedHeight;
  node->AggregatedHeight = 0;
}

void HorizontalContainerListInner::eval(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  HorizontalContainer *self = Content;
  HorizontalContainerListInner *node = this;
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
  
  // eval self.PosY;
  self->PosY = CurrY;
  // eval self.PosX;
  self->PosX = CurrX;
  
  // iterate[left] ElementList {
  //   eval ElementList.PWidth;
  //   eval ElementList.ParentFontStyle;
  //   eval ElementList.CurrX;
  //   eval ElementList.CurrY;
  //   recur ElementList;
  // }
  self->ElementsList->eval(self->FontStyle, CurrX, CurrY, self->Width);
    
  // eval self.Height;
  self->Height = self->ElementsList->MaxHeight;
  
  // iterate[left] Next {
  //   eval Next.CurrY;
  //   eval Next.CurrX;
  //   eval Next.ParentFontStyle;
  //   eval Next.PWidth;
  //   recur Next;
  // }
  node->Next->eval(ParentFontStyle, CurrX, CurrY + self->Height, PWidth);
  // eval self.AggregatedHeight;
  node->AggregatedHeight = self->Height + Next->AggregatedHeight;
}


/*************************************************
 *                   Elements
 *************************************************/
void ElementListNode::eval(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  Content->eval(this, ParentFontStyle, CurrX, CurrY, PWidth);
}

void ElementListEnd::evalNext(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  // no next
}

void ElementListInner::evalNext(FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  Next->eval(ParentFontStyle, CurrX, CurrY, PWidth);
}

/*
 * Generic element
 */
void Element::eval(ElementListNode *node, FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  Element *self = this;

  // eval self.FontStyle1; (adjusted)
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;

  // eval self.PosX; (adjusted)
  self->PosX = CurrX;
  // eval self.PosY; (adjusted)
  self->PosY = CurrY;

  // eval self.WidthRel;
  int WidthRel = self->RelWidth * PWidth;
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

  // iterate[left] Next {
  //   eval Next.CurrY;
  //   eval Next.ParentFontStyle;
  //   eval Next.CurrX;
  //   eval Next.PWidth;
  //   recur Next;
  // }
  node->evalNext(ParentFontStyle, CurrX + this->Width, CurrY, PWidth);
  
  // eval self.Height;
  self->Height = evalHeight();
  // eval self.MaxHeight;
  int nextMaxHeight = node->next()? node->next()->MaxHeight : 0;
  node->MaxHeight = max(self->Height, nextMaxHeight);
  
  // eval self.WidthFlex;
  // no work to do
}

int Element::evalHeight() {
  computeHeights();
  return this->Height;
}


void VerticalContainer::eval(ElementListNode *node, FontInfo ParentFontStyle, int CurrX, int CurrY, int PWidth) {
  // Vertical
  VerticalContainer* self = this;
  // eval self.FontStyle1;
  if (self->FontStyle.Type == (0-1))
    self->FontStyle.Type = ParentFontStyle.Type;
  if (self->FontStyle.Size == (0-1))
    self->FontStyle.Size = ParentFontStyle.Size;
  if (self->FontStyle.Color == (0-1))
    self->FontStyle.Color = ParentFontStyle.Color;

  // adjusted
  // eval self.PosX;
  self->PosX = CurrX;
  // eval self.PosY;
  self->PosY = CurrY;
  
  // eval self.WidthFlex;
  int WidthFlex = self->HorizList->MaxWidth;
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
  // iterate[left] HorizList {
  //   ...
  //   recur HorizList;
  // }
  self->HorizList->eval(self->FontStyle, CurrX, CurrY, self->Width);
  // iterate[left] Next {
  //   ...
  //   recur Next;
  // }
  node->evalNext(ParentFontStyle, CurrX + self->Width, CurrY, PWidth);
  // eval self.Height1;
  self->Height = self->HorizList->AggregatedHeight;
  // eval self.MaxHeight;
  int nextMaxHeight = node->next()? node->next()->MaxHeight : 0;
  node->MaxHeight = max(self->Height, nextMaxHeight);
}


int VerticalContainer::evalHeight() {
  printf("VerticalContainer::evalHeight should not be called");
  return 0;
}