traversal fuse {
  case CDocument {
    iterate[left] PageList {
      eval PageList.ParentFontStyle.Color;
      eval PageList.ParentFontStyle.Type;
      eval PageList.ParentFontStyle.Size;
      recur PageList;
    }
  }
  case CPage {
    eval self.PosY;
    eval self.PosX;
    eval self.FontStyle1.Size;
    eval self.FontStyle1.Color;
    eval self.FontStyle1.Type;
    iterate[left] HorizList {
      eval HorizList.ParentFontStyle.Size;
      eval HorizList.PWidth;
      eval HorizList.CurrX;
      eval HorizList.ParentFontStyle.Color;
      eval HorizList.CurrY;
      eval HorizList.ParentFontStyle.Type;
      recur HorizList;
    }
    iterate[left] Next {
      eval Next.ParentFontStyle.Type;
      eval Next.ParentFontStyle.Color;
      eval Next.ParentFontStyle.Size;
      recur Next;
    }
    eval self.Height1;
    eval self.WidthFlex;
    eval self.Width;
  }
  case CHorizontalContainer {
    eval self.PosX;
    eval self.FontStyle1.Size;
    eval self.FontStyle1.Color;
    eval self.FontStyle1.Type;
    iterate[left] ElementList {
      eval ElementList.ParentFontStyle.Color;
      eval ElementList.CurrX;
      eval ElementList.CurrY;
      eval ElementList.ParentFontStyle.Type;
      eval ElementList.PWidth;
      eval ElementList.ParentFontStyle.Size;
      recur ElementList;
    }
    eval self.Width1;
    eval self.Height1;
    iterate[left] Next {
      eval Next.ParentFontStyle.Size;
      eval Next.ParentFontStyle.Type;
      eval Next.ParentFontStyle.Color;
      eval Next.CurrY;
      eval Next.PWidth;
      eval Next.CurrX;
      recur Next;
    }
    eval self.PosY;
    eval self.MaxWidth;
    eval self.Width2;
    eval self.AggregatedHeight;
  }
  case CNormalElement {
    eval self.Width1;
    eval self.Width2;
    eval self.FontStyle1.Size;
    eval self.Height1;
    eval self.FontStyle1.Type;
    iterate[left] Next {
      eval Next.CurrX;
      eval Next.PWidth;
      eval Next.ParentFontStyle.Size;
      eval Next.ParentFontStyle.Type;
      eval Next.CurrY;
      eval Next.ParentFontStyle.Color;
      recur Next;
    }
    eval self.MaxHeight;
    eval self.AccumulatedWidth;
    eval self.PosX;
    eval self.PosY;
    eval self.FontStyle1.Color;
  }
  case CVerticalContainer {
    eval self.FontStyle1.Size;
    eval self.FontStyle1.Color;
    eval self.FontStyle1.Type;
    iterate[left] HorizList {
      eval HorizList.ParentFontStyle.Size;
      eval HorizList.PWidth;
      eval HorizList.ParentFontStyle.Type;
      eval HorizList.CurrY;
      eval HorizList.CurrX;
      eval HorizList.ParentFontStyle.Color;
      recur HorizList;
    }
    eval self.Width1;
    eval self.Width2;
    iterate[left] Next {
      eval Next.ParentFontStyle.Type;
      eval Next.ParentFontStyle.Color;
      eval Next.ParentFontStyle.Size;
      eval Next.CurrY;
      eval Next.PWidth;
      eval Next.CurrX;
      recur Next;
    }
    eval self.PosX;
    eval self.AccumulatedWidth;
    eval self.PosY;
    eval self.Height1;
    eval self.MaxHeight;
  }
}