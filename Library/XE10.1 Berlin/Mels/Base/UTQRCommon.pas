// *************************************************************************************************
// * ==> UTQRCommon -------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

{**
 @abstract(@name contains common declarations and platform independent generic types, to use when
           the type size should remain constant on any target platform. A good example is the
           MD2/MD3 file reading, where data are written and should always be read as 32 bit values,
           even on 64 bit systems.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRCommon;

interface

type
    {$REGION 'Documentation'}
    {**
     8 bit integer
    }
    {$ENDREGION}
    TQRInt8 = ShortInt;

    {$REGION 'Documentation'}
    {**
     8 bit unsigned integer
    }
    {$ENDREGION}
    TQRUInt8 = Byte;

    {$REGION 'Documentation'}
    {**
     16 bit integer
    }
    {$ENDREGION}
    TQRInt16 = SmallInt;

    {$REGION 'Documentation'}
    {**
     16 bit unsigned integer
    }
    {$ENDREGION}
    TQRUInt16 = Word;

    {$REGION 'Documentation'}
    {**
     32 bit integer
    }
    {$ENDREGION}
    TQRInt32 = LongInt;

    {$REGION 'Documentation'}
    {**
     32 bit unsigned integer
    }
    {$ENDREGION}
    TQRUInt32 = LongWord;

    {$REGION 'Documentation'}
    {**
     64 bit integer
    }
    {$ENDREGION}
    TQRInt64 = Int64;

    {$REGION 'Documentation'}
    {**
     64 bit unsigned integer
    }
    {$ENDREGION}
    TQRUInt64 = UInt64;

    {$REGION 'Documentation'}
    {**
     32 bit floating vlaue
    }
    {$ENDREGION}
    TQRFloat32 = Single;

    {$REGION 'Documentation'}
    {**
     64 bit double value
    }
    {$ENDREGION}
    TQRFloat64 = Double;

    {$REGION 'Documentation'}
    {**
     Pointer to 8 bit integer
    }
    {$ENDREGION}
    PQRInt8 = ^TQRInt8;

    {$REGION 'Documentation'}
    {**
     Pointer to 8 bit unsigned integer
    }
    {$ENDREGION}
    PQRUInt8 = ^TQRUInt8;

    {$REGION 'Documentation'}
    {**
     Pointer to 16 bit integer
    }
    {$ENDREGION}
    PQRInt16 = ^TQRInt16;

    {$REGION 'Documentation'}
    {**
     Pointer to 16 bit unsigned integer
    }
    {$ENDREGION}
    PQRUInt16 = ^TQRUInt16;

    {$REGION 'Documentation'}
    {**
     Pointer to 32 bit integer
    }
    {$ENDREGION}
    PQRInt32 = ^TQRInt32;

    {$REGION 'Documentation'}
    {**
     Pointer to 32 bit unsigned integer
    }
    {$ENDREGION}
    PQRUInt32 = ^TQRUInt32;

    {$REGION 'Documentation'}
    {**
     Pointer to 64 bit integer
    }
    {$ENDREGION}
    PQRInt64 = ^TQRInt64;

    {$REGION 'Documentation'}
    {**
     Pointer to 64 bit unsigned integer
    }
    {$ENDREGION}
    PQRUInt64 = ^TQRUInt64;

    {$REGION 'Documentation'}
    {**
     Pointer to 32 bit floating value
    }
    {$ENDREGION}
    PQRFloat32 = ^TQRFloat32;

    {$REGION 'Documentation'}
    {**
     Pointer to 64 bit double value
    }
    {$ENDREGION}
    PQRFloat64 = ^TQRFloat64;

    {$REGION 'Documentation'}
    {**
     Called when an operation is canceled
    }
    {$ENDREGION}
    TQRIsCanceledEvent = function: Boolean of object;

implementation

end.
