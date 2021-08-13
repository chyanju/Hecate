#ifndef PRETTY_PRINT
#define PRETTY_PRINT

#include "Types.h"
#include "RenderTree.h"

using namespace std;

#define wrap_str(f) '"' << f << '"'

#define key_val(out, k, v)\
(\
  out << wrap_str(k) << ": " << v << "," , \
  (void)0\
)

ostream& operator << (ostream& os, const MEASURE_MODE& m) {
  os << '"';
  switch (m) {
  case ABS: os << "ABS"; break;
  case REL: os << "REL"; break;
  case FLEX: os << "FLEX"; break;
  default: break;
  }
  os << '"';
  return os;
}

ostream& operator << (ostream& os, const String& s) {
  if (s.Content)
    os << wrap_str(s.Content);
  else
    os << wrap_str("");
  return os;
}


ostream& operator << (ostream& os, const ListItems& xs) {
  const String *p = xs.Items;
  os << "[";
  for (int i = 0; i < xs.Size; i++) {
    if (i > 0 and i < xs.Size) {
      os << ", ";
    }
    os << *p;
    p++;
  }
  os << "]";
  return os;
}


ostream& operator << (ostream& os, const FontInfo& fi) {
  os << "FontInfo: ";
  os << "{ ";
  
  os << "Type: ";
  if (fi.Type == -1) os << "null";
  else os << fi.Type;
  os << ", ";

  os << "Size: ";
  if (fi.Size == -1) os << "null";
  else os << fi.Size;
  os << ", ";

  os << "Color: ";
  if (fi.Color == -1) os << "null";
  else os << fi.Color;

  os << " },";
  return os;
}


ostream& Data::format(ostream & out) const {
  out << "Data : { ";
  if (PosX || PosY)
    out << "PosX: " << PosX << ", ";
  if (PosX || PosY)
    out << "PosY: " << PosY << ", ";
  if (Height || Width)
    out << "Width: " << Width << ", ";
  if (Height || Width)
    out << "Height: " << Height << ", ";
  out << "WMode: " << WMode << ", ";
  if (WMode == REL)
    out << "RelWidth: " << RelWidth << ", ";
  if (isFontSet(FontStyle))
    out << FontStyle;
  out << " },";
  return out;
}


ostream& VerticalContainer::format(ostream & out) const { 
  out << "VerticalContainer: {";
  Data::format(out);
  
  out << "HorizList: [" << endl;
  if (HorizList) {
    HorizList->format(out);
  }
  out << "]";
  out << "}";
  return out;
}


ostream& Image::format(ostream & out) const { 
  out << "Image: {";
  Data::format(out);
  key_val(out, "path", wrap_str(path_to_image));
  key_val(out, "ImageOriginalWidth", ImageOriginalWidth);
  key_val(out, "ImageOriginalHeight", ImageOriginalHeight);
  out << "}";
  return out;
}


ostream& List::format(ostream & out) const {
  out << "List: {";
  Data::format(out);
  key_val(out, "Items", Items);
  out << "}";
  return out;
}


ostream& TextBox::format(ostream & out) const {
  out << "TextBox: {";
  Data::format(out);
  key_val(out, "ContentText", ContentText);
  out << "}";
  return out;
}

ostream& ElementListNode::format(ostream & out) const {
  out << "{ " << \
    "AccumulatedWidth: " << AccumulatedWidth << ", " << \
    "MaxHeight: " << MaxHeight << ", " << endl;
  // out << "Content: {"; 
  if (Content) Content->format(out);
  // out << "}";
  out << " }";
  if (next()) {
    out << ", " << endl;
    next()->format(out);
  }
  return out;
}

ostream& HorizontalContainer::format(ostream & out) const {
  out << "HorizontalContainer: { " << endl;
  Data::format(out);
  out << "ElementsList: [" << endl;
  if (ElementsList)
    ElementsList->format(out);
  out << "]" << endl;
  out << "}";
  return out;
}
ostream& HorizontalContainerListNode::format(ostream & out) const {
  out << "{ " << endl;
  out << \
    "MaxWidth: " << MaxWidth << ", " << \
    "AggregatedHeight: " << AggregatedHeight << ", " << endl;
  // out << "Content: {";
  if (Content) Content->format(out);
  // out << "}";
  out << "}";
  if (next()) {
    out << ",";
    next()->format(out);
  }
  return out;
}

ostream& Page::format(ostream & out) const {
  out << "{ ";
  Data::format(out);
  out << "HorizontalContainerList: [";
  if (HorizList) {
    HorizList->format(out);
    out << endl;
  } 
  out << "]";
  out << "}";
  return out;
}

ostream& PageListNode::format(ostream & out) const {
  Content->format(out);
  if (next()) {
    out << ", " << endl;
    next()->format(out);
  }
  return out;
}

ostream& Document::format(ostream & out) const {
  out << "{" << endl;
  out << "Document : { " << endl;
  if (isFontSet(FontStyle))
    out << FontStyle;
  out << "PageList : [" << endl;
  if (PageList)
    PageList->format(out);
  out << "]" << endl;
  out << "}" << endl;
  out << "}" << endl;
  return out;
}

#endif