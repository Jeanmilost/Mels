{**************************************************************************************************
 * ==> UTQRVCLAnimationTimer ---------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module contains a global animation timer based on the VCL TTimer control.   *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRVCLAnimationTimer;

interface

uses System.Classes,
     System.SysUtils,
     UTQRDesignPatterns,
     Vcl.ExtCtrls,
     Winapi.Windows;

type
    {**
    * VCL animation timer messages that can be sent to observers
    *@note Begins to 0 to not interfere with other messages. The allowed range for a new message
    *      of type animation is between 0 and 99
    *}
    EQRVCLAnimationTimerMessages =
    (
        EQR_AM_Animate = 0,
        EQR_AM_Destroying
    );

    {**
    * VCL animation timer message info
    *}
    TQRVCLAnimationTimerMsgInfo = record
        m_ElapsedTime: Double;
    end;

    {**
    * Global animation timer based on the VCL TTimer control
    *}
    TQRVCLAnimationTimer = class sealed (TInterfacedObject, IQRSubject)
        private
            class var m_pInstance:    IQRSubject;
                      m_pTimer:       TTimer;
                      m_pObservers:   TList;
                      m_PreviousTime: Double;
                      m_Info:         TQRVCLAnimationTimerMsgInfo;

            {**
            * Called when animation should be rendered
            *@param pSender - event sender
            *}
            procedure OnAnimate(pSender: TObject);

        public
            { Construction/Destruction }
            constructor Create();
            destructor  Destroy(); override;

            {**
            * Gets animation timer instance, creates one if still not created
            *@return model cache instance
            *}
            class function GetInstance(): IQRSubject; static;

            {**
            * Attaches observer
            *@param pObserver - observer to attach
            *}
            procedure Attach(pObserver: IQRObserver);

            {**
            * Detaches observer
            *@param pObserver - observer to detach
            *}
            procedure Detach(pObserver: IQRObserver);

            {**
            * Notifies all observers about an occurred event
            *@param message - notification message
            *}
            procedure Notify(message: TQRMessage);
    end;

implementation
//------------------------------------------------------------------------------
// TQRVCLAnimationTimer
//------------------------------------------------------------------------------
constructor TQRVCLAnimationTimer.Create();
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    inherited Create;

    // configure internal variables
    m_pObservers   := TList.Create;
    m_PreviousTime := GetTickCount();

    // configure animation timer (an interval of 20 means ~50 fps)
    m_pTimer          := TTimer.Create(nil);
    m_pTimer.Interval := 20;
    m_pTimer.OnTimer  := OnAnimate;
    m_pTimer.Enabled  := True;
end;
//------------------------------------------------------------------------------
destructor TQRVCLAnimationTimer.Destroy();
var
    message: TQRMessage;
begin
    // configure destruction message
    message.m_Type  := NativeUInt(EQR_AM_Destroying);
    message.m_pInfo := nil;

    // notify all observers about destruction
    Notify(message);

    // clear memory
    m_pTimer.Free;
    m_pObservers.Free;

    inherited Destroy;

    m_pInstance := nil;
end;
//------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.OnAnimate(pSender: TObject);
var
    now:     NativeUInt;
    message: TQRMessage;
begin
    // calculate time interval
    now                  :=  GetTickCount();
    m_Info.m_ElapsedTime := (now - m_PreviousTime);
    m_PreviousTime       :=  now;

    // configure animation message
    message.m_Type  := NativeUInt(EQR_AM_Animate);
    message.m_pInfo := @m_Info;

    // notify all observers about animation
    Notify(message);
end;
//------------------------------------------------------------------------------
class function TQRVCLAnimationTimer.GetInstance(): IQRSubject;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
    begin
        // get it
        Result := m_pInstance;
        Exit;
    end;

    // create new singleton instance
    m_pInstance := TQRVCLAnimationTimer.Create;
    Result      := m_pInstance;
end;
//------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.Attach(pObserver: IQRObserver);
begin
    // observer already exists in observers list?
    if (m_pObservers.IndexOf(Pointer(pObserver)) <> -1) then
        Exit;

    // add observer to observers list
    m_pObservers.Add(Pointer(pObserver));
end;
//------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.Detach(pObserver: IQRObserver);
begin
    // remove observer from observers list. NOTE observer list will check if observer exists before
    // trying to remove it, so this check isn't necessary here
    m_pObservers.Remove(Pointer(pObserver));
end;
//------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.Notify(message: TQRMessage);
var
    i:     NativeInt;
    pItem: IQRObserver;
begin
    // nothing to do?
    if (m_pObservers.Count = 0) then
        Exit;

    // iterate through observers to notify
    for i := 0 to m_pObservers.Count - 1 do
    begin
        // get observer to notify
        pItem := IQRObserver(m_pObservers[i]);

        // found it?
        if (not Assigned(pItem)) then
            continue;

        // notify observer about message
        pItem.OnNotified(message);
    end;
end;
//------------------------------------------------------------------------------

end.
