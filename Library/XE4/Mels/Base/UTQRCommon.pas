{**************************************************************************************************
 * ==> UTQRCommon --------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module contains some common declarations.                                   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRCommon;

interface

type
    {**
    * Platform independent generic types, to use when the type size should remain constant on any
    * target platform. A good example is the MD2/MD3 file reading, where data are written and should
    * always be read as 32 bit values, even on 64 bit systems
    *}
    TQRInt8    = ShortInt;
    TQRUInt8   = Byte;
    TQRInt16   = SmallInt;
    TQRUInt16  = Word;
    TQRInt32   = LongInt;
    TQRUInt32  = LongWord;
    TQRInt64   = Int64;
    TQRUInt64  = UInt64;
    TQRFloat32 = Single;
    TQRFloat64 = Double;

    PQRInt8    = ^TQRInt8;
    PQRUInt8   = ^TQRUInt8;
    PQRInt16   = ^TQRInt16;
    PQRUInt16  = ^TQRUInt16;
    PQRInt32   = ^TQRInt32;
    PQRUInt32  = ^TQRUInt32;
    PQRInt64   = ^TQRInt64;
    PQRUInt64  = ^TQRUInt64;
    PQRFloat32 = ^TQRFloat32;
    PQRFloat64 = ^TQRFloat64;

    {**
    * Called when an operation is canceled
    *}
    TQRIsCanceledEvent = function: Boolean of object;

implementation

end.
