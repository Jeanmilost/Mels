// *************************************************************************************************
// * ==> UTQRSmartPointer -------------------------------------------------------------------------*
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
 @abstract(@name provides a ready-to-use smart pointer.)
 @image(Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRSmartPointer;

interface

type
    {$REGION 'Documentation'}
    {**
     This works as an interface containing an anonymous function. This allow the compiler to call
     the Invoke function anonymously while the smart pointer is used, allowing thus to write
     pMyClass.MyProperty instead of pMyClass.Invoke.MyProperty
    }
    {$ENDREGION}
    IQRSmartPointer<T> = reference to function: T;

    {$REGION 'Documentation'}
    {**
     Smart pointer
    }
    {$ENDREGION}
    TQRSmartPointer<T: class, constructor> = class(TInterfacedObject, IQRSmartPointer<T>)
    private
        m_pInstance: T;

    public
        {$REGION 'Documentation'}
        {**
         Constructor
        }
        {$ENDREGION}
        constructor Create; overload; virtual;

        {$REGION 'Documentation'}
        {**
         Constructor
         @param(pInstance New instance to own)
        }
        {$ENDREGION}
        constructor Create(pInstance: T); overload; virtual;

        {$REGION 'Documentation'}
        {**
         Destructor
        }
        {$ENDREGION}
        destructor Destroy; override;

        {$REGION 'Documentation'}
        {**
         Invokes the owned class (needed to be used by the IQRSmartPointer interface)
         @return(Owned class)
        }
        {$ENDREGION}
        function Invoke: T; virtual;

        {$REGION 'Documentation'}
        {**
         Gets the owned class
         @return(Owned class)
        }
        {$ENDREGION}
        function Get: T; virtual;

        {$REGION 'Documentation'}
        {**
         Releases the owned class
         @return(Released owned class)
        }
        {$ENDREGION}
        function Release: T; virtual;

        {$REGION 'Documentation'}
        {**
         Reset the current instance of owned class and replace by a new one
         @param(pNewInstance New instance to own)
        }
        {$ENDREGION}
        procedure Reset(pNewInstance: T); virtual;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRSmartPointer
//--------------------------------------------------------------------------------------------------
constructor TQRSmartPointer<T>.Create;
begin
    inherited Create;

    m_pInstance := T.Create;
end;
//--------------------------------------------------------------------------------------------------
constructor TQRSmartPointer<T>.Create(pInstance: T);
begin
    inherited Create;

    m_pInstance := pInstance;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRSmartPointer<T>.Destroy;
begin
    m_pInstance.Free;
    m_pInstance := nil;

    inherited;
end;
//--------------------------------------------------------------------------------------------------
function TQRSmartPointer<T>.Invoke: T;
begin
    Result := m_pInstance;
end;
//--------------------------------------------------------------------------------------------------
function TQRSmartPointer<T>.Get: T;
begin
    Result := m_pInstance;
end;
//--------------------------------------------------------------------------------------------------
function TQRSmartPointer<T>.Release: T;
begin
    Result      := m_pInstance;
    m_pInstance := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRSmartPointer<T>.Reset(pNewInstance: T);
begin
    // delete previous instance if exists
    m_pInstance.Free;

    // then replace by new instance
    m_pInstance := pNewInstance;
end;
//--------------------------------------------------------------------------------------------------

end.
