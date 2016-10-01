{**************************************************************************************************
 * ==> UTQRDesignerHook --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Hook to listen and resend design time messages.                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRDesignerHook;

interface

uses System.Classes,
     UTQRDesignPatterns,
     Vcl.Controls,
     Winapi.Messages,
     Winapi.Windows;

type
    {**
    * Designer hook messages that can be sent to observers
    *@note Begins to 100 to not interfere with other messages. The allowed range for a new message
    *      of type designer hook is between 100 and 199
    *}
    EQRDesignerHookMessages =
    (
        EQR_DH_Message = 100,
        EQR_DH_Destroying
    );

    {**
    * Designer hook message info
    *}
    TQRDesignerHookMsgInfo = record
        m_Message: NativeUInt;
        m_WParam:  WPARAM;
        m_LParam:  LPARAM;
    end;

    {**
    * Hook for Embarcadero RAD Studio designer
    *@note Descendent of TComponent to allow the hook to receive notifications
    *}
    TQRDesignerHook = class sealed (TComponent, IQRSubject)
        private
            class var m_pInstance:      TQRDesignerHook;
                      m_pHookedControl: TWinControl;
                      m_pObservers:     TList;
                      m_Info:           TQRDesignerHookMsgInfo;
                      m_Filters:        array of NativeUInt;
                      m_hPrevWndProc:   TWndMethod;

            { Construction/Destruction }
            constructor Create();  reintroduce;
            destructor  Destroy(); reintroduce;
            procedure   Free();    reintroduce;

            {**
            * Called when a Windows message was received from hooked control
            *@param pSender - event sender
            *}
            procedure OnMessage(var message: TMessage);

        protected
            {**
            * Called when observed component sent a notification
            *@param pComponent - component that sent the notification
            *@param operation - operation that observed component is currently doing
            *}
            procedure Notification(pComponent: TComponent; operation: TOperation); override;

        public
            {**
            * Gets designer hook instance, creates one if still not created
            *@return model cache instance
            *}
            class function GetInstance(): TQRDesignerHook; static;

            {**
            * Deletes designer hook instance
            *@note This function is automatically called when unit is released
            *}
            class procedure DeleteInstance(); static;

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
            * Sets the designer control to hook
            *@param pControl - designer control to hook
            *}
            procedure SetHookedControl(pControl: TWinControl);

            {**
            * Adds a filter
            *@param filter - message filter to add
            *}
            procedure AddFilter(filter: NativeUInt);

            {**
            * Notifies all observers about an occurred event
            *@param message - notification message
            *}
            procedure Notify(message: TQRMessage);
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRDesignerHook
//--------------------------------------------------------------------------------------------------
constructor TQRDesignerHook.Create();
begin
    // not really designed to be a component, so can set his owner to nil
    inherited Create(nil);

    // configure internal variables
    m_pObservers     := TList.Create;
    m_pHookedControl := nil;
    m_hPrevWndProc   := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRDesignerHook.Destroy();
var
    message: TQRMessage;
begin
    // restore original Windows procedure to hooked control
    if (Assigned(m_pHookedControl)) then
        if (Assigned(m_hPrevWndProc)) then
            m_pHookedControl.WindowProc := m_hPrevWndProc
        else
            m_pHookedControl.WindowProc := nil;

    // configure destruction message
    message.m_Type  := NativeUInt(EQR_DH_Destroying);
    message.m_pInfo := nil;

    // notify all observers about destruction
    Notify(message);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Free();
begin
    // check if self is already deleted, delete itself if not
    if (Assigned(Self)) then
        Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.OnMessage(var message: TMessage);
var
    hookMsg:  TQRMessage;
    count, i: NativeUInt;
    found:    Boolean;
begin
    // don't send notifications if hooked control is deleting
    if (csDestroying in m_pHookedControl.ComponentState) then
        Exit;

    count := Length(m_Filters);
    found := (count = 0);

    // is message filtered?
    for i := 0 to count - 1 do
        if (m_Filters[i] = message.Msg) then
        begin
            found := True;
            break;
        end;

    // do process message?
    if (found) then
    begin
        // copy Windows message to hook info structure
        m_Info.m_Message := message.Msg;
        m_Info.m_WParam  := message.WParam;
        m_Info.m_LParam  := message.LParam;

        // configure hook message
        hookMsg.m_Type  := NativeUInt(EQR_DH_Message);
        hookMsg.m_pInfo := @m_Info;

        // notify all observers about animation
        Notify(hookMsg);
    end;

    if (Assigned(m_hPrevWndProc)) then
        m_hPrevWndProc(message);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Notification(pComponent: TComponent; operation: TOperation);
begin
    inherited Notification(pComponent, operation);

    // is hooked designer component currently deleting?
    if (pComponent = m_pHookedControl) and (operation = opRemove) then
    begin
        // restore original message loop
        if (Assigned(m_pHookedControl)) then
            if (Assigned(m_hPrevWndProc)) then
                m_pHookedControl.WindowProc := m_hPrevWndProc
            else
                m_pHookedControl.WindowProc := nil;

        // release hook
        m_pHookedControl := nil;
        m_hPrevWndProc   := nil;
    end;
end;
//--------------------------------------------------------------------------------------------------
class function TQRDesignerHook.GetInstance(): TQRDesignerHook;
var
    pInstance: TQRDesignerHook;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
    begin
        // get it
        Result := m_pInstance;
        Exit;
    end;

    // create new singleton instance
    pInstance := TQRDesignerHook.Create;

    // another thread already created the singleton instance?
    if (InterlockedCompareExchangePointer(Pointer(m_pInstance), pInstance, nil) <> nil) then
        // only one instance is allowed, and another instance was already created by another
        // thread, so delete above created one and use the available instance
        pInstance.Free
    else
        // still not created, set the newly created instance
        m_pInstance := pInstance;

    // get newly created instance
    Result := m_pInstance;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRDesignerHook.DeleteInstance();
begin
    m_pInstance.Free;
    m_pInstance := nil;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Attach(pObserver: IQRObserver);
begin
    // observer already exists in observers list?
    if (m_pObservers.IndexOf(Pointer(pObserver)) <> -1) then
        Exit;

    // add observer to observers list
    m_pObservers.Add(Pointer(pObserver));
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Detach(pObserver: IQRObserver);
begin
    // remove observer from observers list. NOTE observer list will check if observer exists before
    // trying to remove it, so this check isn't necessary here
    m_pObservers.Remove(Pointer(pObserver));
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.SetHookedControl(pControl: TWinControl);
begin
    // is control already hooked?
    if (m_pHookedControl = pControl) then
        Exit;

    // another control was already hooked?
    if (Assigned(m_pHookedControl)) then
        if (Assigned(m_hPrevWndProc)) then
            m_pHookedControl.WindowProc := m_hPrevWndProc
        else
            m_pHookedControl.WindowProc := nil;

    // no new control to hook?
    if (not Assigned(pControl)) then
    begin
        m_pHookedControl := nil;
        m_hPrevWndProc   := nil;
        Exit;
    end;

    // hook the control
    m_pHookedControl            := pControl;
    m_hPrevWndProc              := m_pHookedControl.WindowProc;
    m_pHookedControl.WindowProc := OnMessage;

    // register this control in hooked one to receive notifications when it will be deleted
    m_pHookedControl.FreeNotification(Self);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.AddFilter(filter: NativeUInt);
var
    count, i: NativeInt;
begin
    count := Length(m_Filters);

    // is filter already added?
    for i := 0 to count - 1 do
        if (m_Filters[i] = filter) then
            Exit;

    // add filter to list
    SetLength(m_Filters, count + 1);
    m_Filters[count] := filter;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Notify(message: TQRMessage);
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
//--------------------------------------------------------------------------------------------------

initialization
//--------------------------------------------------------------------------------------------------
// TQRVCLAnimationTimer
//--------------------------------------------------------------------------------------------------
begin
    // initialize instance to default when application opens
    TQRDesignerHook.m_pInstance := nil;
end;
//--------------------------------------------------------------------------------------------------

finalization
//--------------------------------------------------------------------------------------------------
// TQRVCLAnimationTimer
//--------------------------------------------------------------------------------------------------
begin
    // free instance when application closes
    TQRDesignerHook.DeleteInstance();
end;
//--------------------------------------------------------------------------------------------------

end.
