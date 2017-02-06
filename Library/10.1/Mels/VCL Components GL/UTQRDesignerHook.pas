// *************************************************************************************************
// * ==> UTQRDesignerHook -------------------------------------------------------------------------*
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
 @abstract(@name provides a hook to listen and resend several design time messages. This is required
           because some events, as the scrolling events, are required by the Mels components to e.g.
           refresh their interface, however these messages are never dispatched in the Windows
           message loop.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRDesignerHook;

interface

uses System.Classes,
     System.SysUtils,
     UTQRDesignPatterns,
     Vcl.Controls,
     Winapi.Messages,
     Winapi.Windows;

type
    {$REGION 'Documentation'}
    {**
     Designer hook messages that can be sent to observers
     @value(EQR_DH_Message Message notifying that a monitored message was sent in the message loop
                           of the hooked control)
     @value(EQR_DH_Destroying Message notifying that the designer hook is being destroyed)
     @br @bold(NOTE) These values begin on 100 to not interfere with other messages. The allowed
                     range for a new designer hook message is between 100 and 199
    }
    {$ENDREGION}
    EQRDesignerHookMessages =
    (
        EQR_DH_Message = 100,
        EQR_DH_Destroying
    );

    {$REGION 'Documentation'}
    {**
     Designer hook message info
    }
    {$ENDREGION}
    TQRDesignerHookMsgInfo = record
        {$REGION 'Documentation'}
        {**
         The message received in the hooked message loop
        }
        {$ENDREGION}
        m_Message: NativeUInt;

        {$REGION 'Documentation'}
        {**
         The message WParam value
        }
        {$ENDREGION}
        m_WParam: WPARAM;

        {$REGION 'Documentation'}
        {**
         The message LParam value
        }
        {$ENDREGION}
        m_LParam: LPARAM;
    end;

    {$REGION 'Documentation'}
    {**
     Hook for Embarcadero RAD Studio designer
     @br @bold(NOTE) Descendent of TComponent to allow the hook to receive notifications
    }
    {$ENDREGION}
    TQRDesignerHook = class sealed (TComponent, IQRSubject)
        private
            class var m_pInstance:      TQRDesignerHook;
                      m_pHookedControl: TWinControl;
                      m_pObservers:     TList;
                      m_Info:           TQRDesignerHookMsgInfo;
                      m_Filters:        array of NativeUInt;
                      m_hPrevWndProc:   TWndMethod;

            {$REGION 'Documentation'}
            {**
             Called when a Windows message is received from hooked control
             @param(pSender Event sender)
            }
            {$ENDREGION}
            procedure OnMessage(var message: TMessage);

        protected
            {$REGION 'Documentation'}
            {**
             Called when observed component sent a notification
             @param(pComponent Component that sent the notification)
             @param(operation Operation that observed component is currently doing)
            }
            {$ENDREGION}
            procedure Notification(pComponent: TComponent; operation: TOperation); override;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; reintroduce;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets designer hook instance, creates one if still not created
             @return(Model cache instance)
            }
            {$ENDREGION}
            class function GetInstance: TQRDesignerHook; static;

            {$REGION 'Documentation'}
            {**
             Deletes designer hook instance
             @br @bold(NOTE) This function is automatically called when unit is released
            }
            {$ENDREGION}
            class procedure DeleteInstance; static;

            {$REGION 'Documentation'}
            {**
             Attaches observer
             @param(pObserver Observer to attach)
            }
            {$ENDREGION}
            procedure Attach(pObserver: IQRObserver);

            {$REGION 'Documentation'}
            {**
             Detaches observer
             @param(pObserver Observer to detach)
            }
            {$ENDREGION}
            procedure Detach(pObserver: IQRObserver);

            {$REGION 'Documentation'}
            {**
             Sets the designer control to hook
             @param(pControl Designer control to hook)
            }
            {$ENDREGION}
            procedure SetHookedControl(pControl: TWinControl);

            {$REGION 'Documentation'}
            {**
             Adds a filter
             @param(filter Message filter to add)
            }
            {$ENDREGION}
            procedure AddFilter(filter: NativeUInt);

            {$REGION 'Documentation'}
            {**
             Notifies all observers about an occurred event
             @param(message Notification message)
            }
            {$ENDREGION}
            procedure Notify(message: TQRMessage);
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRDesignerHook
//--------------------------------------------------------------------------------------------------
constructor TQRDesignerHook.Create;
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    // not really designed to be a component, so can set his owner to nil
    inherited Create(nil);

    // configure internal variables
    m_pObservers     := TList.Create;
    m_pHookedControl := nil;
    m_hPrevWndProc   := nil;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRDesignerHook.Destroy;
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
procedure TQRDesignerHook.OnMessage(var message: TMessage);
var
    hookMsg: TQRMessage;
    filter:  NativeUInt;
    found:   Boolean;
begin
    // don't send notifications if hooked control is deleting
    if (csDestroying in m_pHookedControl.ComponentState) then
        Exit;

    found := False;

    // is message filtered?
    for filter in m_Filters do
        if (filter = message.Msg) then
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
class function TQRDesignerHook.GetInstance: TQRDesignerHook;
var
    pInstance: TQRDesignerHook;
begin
    // is singleton instance already initialized?
    if (Assigned(m_pInstance)) then
        // get it
        Exit(m_pInstance);

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
class procedure TQRDesignerHook.DeleteInstance;
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
    existingFilter: NativeUInt;
    index:          NativeInt;
begin
    // is filter already added?
    for existingFilter in m_Filters do
        if (existingFilter = filter) then
            Exit;

    // add filter to list
    index := Length(m_Filters);
    SetLength(m_Filters, index + 1);
    m_Filters[index] := filter;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRDesignerHook.Notify(message: TQRMessage);
var
    pObject: Pointer;
    pItem:   IQRObserver;
begin
    // iterate through observers to notify
    for pObject in m_pObservers do
    begin
        // get observer to notify
        pItem := IQRObserver(pObject);

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
    TQRDesignerHook.DeleteInstance;
end;
//--------------------------------------------------------------------------------------------------

end.
