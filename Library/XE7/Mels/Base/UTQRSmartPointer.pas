{**************************************************************************************************
 * ==> UTQRSmartPointer --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides a smart pointer implementation                              *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRSmartPointer;

interface

type
    {**
    * This works as an interface containing an anonymous function. This allow the compiler to call
    * the Invoke function anonymously while the smart pointer is used, allowing thus to write
    * pMyClass.MyProperty instead of pMyClass.Invoke.MyProperty
    *}
    IQRSmartPointer<T> = reference to function: T;

    {**
    * Smart pointer
    *}
    TQRSmartPointer<T: class, constructor> = class(TInterfacedObject, IQRSmartPointer<T>)
    private
        m_pInstance: T;

    public
        {**
        * Constructor
        *}
        constructor Create; overload; virtual;

        {**
        * Constructor
        *@param pInstance - new instance to own
        *}
        constructor Create(pInstance: T); overload; virtual;

        {**
        * Destructor
        *}
        destructor Destroy; override;

        {**
        * Invokes the owned class (needed to be used by the IQRSmartPointer interface)
        *@return owned class
        *}
        function Invoke: T; virtual;

        {**
        * Gets the owned class
        *@return owned class
        *}
        function Get: T; virtual;

        {**
        * Releases the owned class
        *@return released owned class
        *}
        function Release: T; virtual;

        {**
        * Reset the current instance of owned class and replace by a new one
        *@param pNewInstance - new instance to own
        *}
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
