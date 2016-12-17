// *************************************************************************************************
// * ==> UTQRVCLAnimationTimer --------------------------------------------------------------------*
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
 @abstract(@name provides a global animation timer based on the VCL TTimer control.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRVCLAnimationTimer;

interface

uses Classes,
     SysUtils,
     ExtCtrls,
     Windows,
     UTQRDesignPatterns;

type
    {$REGION 'Documentation'}
    {**
     VCL animation timer messages that can be sent to observers
     @value(EQR_AM_Animate Message notifying that a new animation frame should be generated)
     @value(EQR_AM_Destroying Message notifying that the animation timer is being destroyed)
     @br @bold(NOTE) These values begin on 0 to not interfere with other messages. The allowed range
                     for a new animation timer message is between 0 and 99
    }
    {$ENDREGION}
    EQRVCLAnimationTimerMessages =
    (
        EQR_AM_Animate = 0,
        EQR_AM_Destroying
    );

    {$REGION 'Documentation'}
    {**
     VCL animation timer message info
    }
    {$ENDREGION}
    TQRVCLAnimationTimerMsgInfo = record
        {$REGION 'Documentation'}
        {**
         Elapsed time since last draw in milliseconds
        }
        {$ENDREGION}
        m_ElapsedTime: Double;
    end;

    {$REGION 'Documentation'}
    {**
     Global animation timer based on the VCL TTimer control
    }
    {$ENDREGION}
    TQRVCLAnimationTimer = class sealed (TInterfacedObject, IQRSubject)
        private
            class var m_pInstance:    IQRSubject;
                      m_pTimer:       TTimer;
                      m_pObservers:   TList;
                      m_PreviousTime: Double;
                      m_Info:         TQRVCLAnimationTimerMsgInfo;

            {$REGION 'Documentation'}
            {**
             Called when animation should be rendered
             @param(pSender Event sender)
            }
            {$ENDREGION}
            procedure OnAnimate(pSender: TObject);

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets animation timer instance, creates one if still not created
             @return(Model cache instance)
            }
            {$ENDREGION}
            class function GetInstance: IQRSubject; static;

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
             Notifies all observers about an occurred event
             @param(message Notification message)
            }
            {$ENDREGION}
            procedure Notify(message: TQRMessage);
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRVCLAnimationTimer
//--------------------------------------------------------------------------------------------------
constructor TQRVCLAnimationTimer.Create;
begin
    // singleton was already initialized?
    if (Assigned(m_pInstance)) then
        raise Exception.Create('Cannot create many instances of a singleton class');

    inherited Create;

    // configure internal variables
    m_pObservers   := TList.Create;
    m_PreviousTime := GetTickCount;

    // configure animation timer (an interval of 20 means ~50 fps)
    m_pTimer          := TTimer.Create(nil);
    m_pTimer.Interval := 20;
    m_pTimer.OnTimer  := @OnAnimate;
    m_pTimer.Enabled  := True;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLAnimationTimer.Destroy;
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.OnAnimate(pSender: TObject);
var
    now:     NativeUInt;
    message: TQRMessage;
begin
    // calculate time interval
    now                  :=  GetTickCount;
    m_Info.m_ElapsedTime := (now - m_PreviousTime);
    m_PreviousTime       :=  now;

    // configure animation message
    message.m_Type  := NativeUInt(EQR_AM_Animate);
    message.m_pInfo := @m_Info;

    // notify all observers about animation
    Notify(message);
end;
//--------------------------------------------------------------------------------------------------
class function TQRVCLAnimationTimer.GetInstance: IQRSubject;
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
//--------------------------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.Attach(pObserver: IQRObserver);
begin
    // observer already exists in observers list?
    if (m_pObservers.IndexOf(Pointer(pObserver)) <> -1) then
        Exit;

    // add observer to observers list
    m_pObservers.Add(Pointer(pObserver));
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLAnimationTimer.Detach(pObserver: IQRObserver);
begin
    // remove observer from observers list. NOTE observer list will check if observer exists before
    // trying to remove it, so this check isn't necessary here
    m_pObservers.Remove(Pointer(pObserver));
end;
//--------------------------------------------------------------------------------------------------
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
//--------------------------------------------------------------------------------------------------

end.
