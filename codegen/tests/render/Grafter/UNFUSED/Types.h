#ifndef TYPES
#define TYPES

#include <iostream>

using namespace std;


enum MEASURE_MODE { ABS, REL, FLEX };

struct String {
  char *Content;
  int Length=0;
};


struct ListItems {
public:
  String *Items;
  int Size=0;
};


struct FontInfo {
public:
  unsigned int Type = -1;
  unsigned int Size = -1;
  unsigned int Color = -1;
};

bool isFontSet(const FontInfo& FontStyle) {
  return !(FontStyle.Type == -1 && FontStyle.Size == -1 && FontStyle.Color == -1);
}



class Data {
public:
  int PosX = 0;
  int PosY = 0;
  int Height = 0;
  int Width = 0;
  float RelWidth;
  MEASURE_MODE WMode;
  FontInfo FontStyle;
  unsigned int BackgroundColor = 0;
  virtual ostream& format(ostream & out) const;
  friend ostream& operator << (ostream& out, const Data& data) {
    return data.format(out);
  }
};


template <typename T>
class LinkedList {
public:
  virtual T const *next() const = 0;
  virtual T *_next() const { return NULL; }
};

#endif