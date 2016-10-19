// *************************************************************************************************
// * ==> UTQRThreading ----------------------------------------------------------------------------*
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
 @abstract(@name provides the basic features to create threaded jobs and to send them to a worker.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit UTQRThreading;

interface

uses System.Classes,
     System.SysUtils,
     System.SyncObjs,
     Winapi.Windows;

type
    {$REGION 'Documentation'}
    {**
     Job status enumeration
     @value(EQR_JS_Unknown Indicates that the job was still not added)
     @value(EQR_JS_NotStarted Indicates that the job was added to job list, but still not started)
     @value(EQR_JS_Processing Indicates that the job was started)
     @value(EQR_JS_Done Indicates that the job is done)
     @value(EQR_JS_Canceled Indicates that the job was canceled)
     @value(EQR_JS_Error Indicates that the job is done but an error occurred)
    }
    {$ENDREGION}
    EQRThreadJobStatus =
    (
        EQR_JS_Unknown = 0,
        EQR_JS_NotStarted,
        EQR_JS_Processing,
        EQR_JS_Done,
        EQR_JS_Canceled,
        EQR_JS_Error
    );

    {$REGION 'Documentation'}
    {**
     Thread job helper
    }
    {$ENDREGION}
    TQRThreadJobHelper = record
        {$REGION 'Documentation'}
        {**
         Converts job status enumeration to string
         @param(status Job status to convert)
         @return(Job status as string)
        }
        {$ENDREGION}
        class function JobStatusToStr(status: EQRThreadJobStatus): UnicodeString; static;
    end;

    {$REGION 'Documentation'}
    {**
     Basic interface for threaded jobs (NOTE using class instead of interface to avoid to use the
     reference counting. Here an interface is just a basic contract that certify that several
     functions should be implemented and to access them in a generic way)
    }
    {$ENDREGION}
    TQRThreadJob = class
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Processes the job
             @return(@true on success, otherwise @false)
            }
            {$ENDREGION}
            function Process: Boolean; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Cancels the job
            }
            {$ENDREGION}
            procedure Cancel; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Gets the job status
             @return(The job status)
            }
            {$ENDREGION}
            function GetStatus: EQRThreadJobStatus; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Sets the job status
             @param(status The job status)
            }
            {$ENDREGION}
            procedure SetStatus(status: EQRThreadJobStatus); virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Thread lock interface (NOTE using class instead of interface to avoid to use the reference
     counting. Here an interface is just a basic contract that certify that several functions
     should be implemented and to access them in a generic way)
    }
    {$ENDREGION}
    TQRThreadLock = class
        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Locks a code block to prevent access by other threads
            }
            {$ENDREGION}
            procedure Lock; virtual; abstract;

            {$REGION 'Documentation'}
            {**
             Unlocks a code block to allow access by other threads
            }
            {$ENDREGION}
            procedure Unlock; virtual; abstract;
    end;

    {$REGION 'Documentation'}
    {**
     Multi threading lock using Windows features
    }
    {$ENDREGION}
    TQRWinThreadLock = class(TQRThreadLock)
        private
            m_pCritical: RTL_CRITICAL_SECTION;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Locks a code block to prevent access by other threads
            }
            {$ENDREGION}
            procedure Lock; override;

            {$REGION 'Documentation'}
            {**
             Unlocks a code block to allow access by other threads
            }
            {$ENDREGION}
            procedure Unlock; override;
    end;

    {$REGION 'Documentation'}
    {**
     Multi threading lock using VCL features
    }
    {$ENDREGION}
    TQRVCLThreadLock = class(TQRThreadLock)
        private
            m_pCritical: TCriticalSection;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Locks a code block to prevent access by other threads
            }
            {$ENDREGION}
            procedure Lock; override;

            {$REGION 'Documentation'}
            {**
             Unlocks a code block to allow access by other threads
            }
            {$ENDREGION}
            procedure Unlock; override;
    end;

    {$REGION 'Documentation'}
    {**
     VCL thread worker job, processes a job in a worker thread using VCL
    }
    {$ENDREGION}
    TQRVCLThreadWorkerJob = class(TQRThreadJob)
        private
            m_Status: EQRThreadJobStatus;

        protected
            m_pLock: TQRVCLThreadLock;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; override;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Gets the job status
             @return(The job status)
            }
            {$ENDREGION}
            function GetStatus: EQRThreadJobStatus; override;

            {$REGION 'Documentation'}
            {**
             Sets the job status
             @param(status The job status)
            }
            {$ENDREGION}
            procedure SetStatus(status: EQRThreadJobStatus); override;
    end;

    {$REGION 'Documentation'}
    {**
     Called when a job is about to be processed
     @param(pJob Job to process)
    }
    {$ENDREGION}
    TQRThreadJobProcessEvent = procedure(pJob: TQRThreadJob) of object;

    {$REGION 'Documentation'}
    {**
     Called when a job is done
     @param(pJob Done job)
    }
    {$ENDREGION}
    TQRThreadJobDoneEvent = procedure(pJob: TQRThreadJob) of object;

    {$REGION 'Documentation'}
    {**
     Called when a job is canceled
     @param(pJob Canceled job)
    }
    {$ENDREGION}
    TQRThreadJobCancelEvent = procedure(pJob: TQRThreadJob) of object;

    {$REGION 'Documentation'}
    {**
     Called when a job is idle
    }
    {$ENDREGION}
    TQRThreadJobIdleEvent = procedure of object;

    {$REGION 'Documentation'}
    {**
     Windows thread worker, executes a list of jobs, one by one, until all jobs are processed
    }
    {$ENDREGION}
    TQRVCLThreadWorker = class(TThread)
        private
            m_pLock:          TQRVCLThreadLock;
            m_pJobs:          TList;
            m_pProcessingJob: TQRThreadJob;
            m_Idle:           Boolean;
            m_IsIdle:         Boolean;
            m_Canceled:       Boolean;
            m_fOnProcess:     TQRThreadJobProcessEvent;
            m_fOnDone:        TQRThreadJobDoneEvent;
            m_fOnCanceled:    TQRThreadJobCancelEvent;
            m_fOnIdle:        TQRThreadJobIdleEvent;
            m_Started:        Boolean;

        protected
            {$REGION 'Documentation'}
            {**
             Gets currently processing job
             @return(Currently processing job, @nil if no job is currently processing)
            }
            {$ENDREGION}
            function GetProcessingJob: TQRThreadJob; virtual;

            {$REGION 'Documentation'}
            {**
             Gets the OnProcess callback
             @return(OnProcess callback)
            }
            {$ENDREGION}
            function GetOnProcess: TQRThreadJobProcessEvent; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the OnProcess callback
             @param(fOnProcess OnProcess callback)
            }
            {$ENDREGION}
            procedure SetOnProcess(fOnProcess: TQRThreadJobProcessEvent); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the OnDone callback
             @return(OnDone callback)
            }
            {$ENDREGION}
            function GetOnDone: TQRThreadJobDoneEvent; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the OnDone callback
             @param(fOnDone OnDone callback)
            }
            {$ENDREGION}
            procedure SetOnDone(fOnDone: TQRThreadJobDoneEvent); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the OnCanceled callback
             @return(OnCanceled callback)
            }
            {$ENDREGION}
            function GetOnCanceled: TQRThreadJobCancelEvent; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the OnCanceled callback
             @param(OnCanceled OnCanceled callback)
            }
            {$ENDREGION}
            procedure SetOnCanceled(fOnCanceled: TQRThreadJobCancelEvent); virtual;

            {$REGION 'Documentation'}
            {**
             Gets the OnIdle callback
             @return(OnIdle callback)
            }
            {$ENDREGION}
            function GetOnIdle: TQRThreadJobIdleEvent; virtual;

            {$REGION 'Documentation'}
            {**
             Sets the OnIdle callback
             @param(OnIdle OnIdle callback)
            }
            {$ENDREGION}
            procedure SetOnIdle(fOnIDle: TQRThreadJobIdleEvent); virtual;

            {$REGION 'Documentation'}
            {**
             Executes work
            }
            {$ENDREGION}
            procedure Execute; override;

            {$REGION 'Documentation'}
            {**
             Notifies that job is about to be processed
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnProcessNotify; virtual;

            {$REGION 'Documentation'}
            {**
             Notifies that job is done
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnDoneNotify; virtual;

            {$REGION 'Documentation'}
            {**
             Notifies that job is canceled
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnCanceledNotify; virtual;

            {$REGION 'Documentation'}
            {**
             Notifies that worker is idle
             @br @bold(NOTE) This function is executed on the calling thread side
            }
            {$ENDREGION}
            procedure OnIdleNotify; virtual;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Adds job to process list
             @param(pJob Job to add)
            }
            {$ENDREGION}
            procedure AddJob(pJob: TQRThreadJob); virtual;

            {$REGION 'Documentation'}
            {**
             Deletes job from process list
             @param(pJob Job to delete)
             @param(doCancel If @true, job will be canceled before deleted)
            }
            {$ENDREGION}
            procedure DeleteJob(pJob: TQRThreadJob; doCancel: Boolean = True); virtual;

            {$REGION 'Documentation'}
            {**
             Pauses or resumes thread activity
             @param(value If @true, thread will become idle, otherwise resume from idle state)
            }
            {$ENDREGION}
            procedure MakeIdle(value: Boolean); virtual;

            {$REGION 'Documentation'}
            {**
             Cancels all the jobs
            }
            {$ENDREGION}
            procedure Cancel; virtual;

            {$REGION 'Documentation'}
            {**
             Check if worker was canceled
             @return(@true if worker was canceled, otherwise @false)
            }
            {$ENDREGION}
            function IsCanceled: Boolean; virtual;

        // Properties
        public
            {$REGION 'Documentation'}
            {**
             Gets the processing job
            }
            {$ENDREGION}
            property ProcessingJob: TQRThreadJob read GetProcessingJob;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnProcess event
            }
            {$ENDREGION}
            property OnProcess: TQRThreadJobProcessEvent read GetOnProcess write SetOnProcess;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnDone event
            }
            {$ENDREGION}
            property OnDone: TQRThreadJobDoneEvent read GetOnDone write SetOnDone;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnCanceled event
            }
            {$ENDREGION}
            property OnCanceled: TQRThreadJobCancelEvent read GetOnCanceled write SetOnCanceled;

            {$REGION 'Documentation'}
            {**
             Gets or sets the OnIdle event
            }
            {$ENDREGION}
            property OnIdle: TQRThreadJobIdleEvent read GetOnIdle write SetOnIdle;

            {$REGION 'Documentation'}
            {**
             Gets if the worker started
             @br @bold(NOTE) This property is implemented by the VCL in RAD Studio XE7 or above
            }
            {$ENDREGION}
            property Started: Boolean read m_Started;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRThreadJobHelper
//--------------------------------------------------------------------------------------------------
class function TQRThreadJobHelper.JobStatusToStr(status: EQRThreadJobStatus): UnicodeString;
begin
    case status of
        EQR_JS_Unknown:    Result := 'EQR_JS_Unknown';
        EQR_JS_NotStarted: Result := 'EQR_JS_NotStarted';
        EQR_JS_Processing: Result := 'EQR_JS_Processing';
        EQR_JS_Done:       Result := 'EQR_JS_Done';
        EQR_JS_Canceled:   Result := 'EQR_JS_Canceled';
        EQR_JS_Error:      Result := 'EQR_JS_Error';
    else
        Result := '<#ERROR (' + IntToStr(NativeInt(status)) + ')>';
    end;
end;
//--------------------------------------------------------------------------------------------------
// TQRThreadJob
//--------------------------------------------------------------------------------------------------
constructor TQRThreadJob.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRThreadJob.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRThreadLock
//--------------------------------------------------------------------------------------------------
constructor TQRThreadLock.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRThreadLock.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
// TQRWinThreadLock
//--------------------------------------------------------------------------------------------------
constructor TQRWinThreadLock.Create;
begin
    inherited Create;

    InitializeCriticalSection(m_pCritical);
end;
//--------------------------------------------------------------------------------------------------
destructor TQRWinThreadLock.Destroy;
begin
    DeleteCriticalSection(m_pCritical);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRWinThreadLock.Lock;
begin
    EnterCriticalSection(m_pCritical);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRWinThreadLock.Unlock;
begin
    LeaveCriticalSection(m_pCritical);
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLThreadLock
//--------------------------------------------------------------------------------------------------
constructor TQRVCLThreadLock.Create;
begin
    inherited Create;

    m_pCritical := TCriticalSection.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLThreadLock.Destroy;
begin
    m_pCritical.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadLock.Lock;
begin
    m_pCritical.Acquire;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadLock.Unlock;
begin
    m_pCritical.Release;
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLThreadWorkerJob
//--------------------------------------------------------------------------------------------------
constructor TQRVCLThreadWorkerJob.Create;
begin
    inherited Create;

    m_Status := EQR_JS_Unknown;
    m_pLock  := TQRVCLThreadLock.Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLThreadWorkerJob.Destroy;
begin
    m_pLock.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorkerJob.GetStatus: EQRThreadJobStatus;
var
    status: EQRThreadJobStatus;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    status := m_Status;
    m_pLock.Unlock;

    Result := status;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorkerJob.SetStatus(status: EQRThreadJobStatus);
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    m_Status := status;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
// TQRVCLThreadWorker
//--------------------------------------------------------------------------------------------------
constructor TQRVCLThreadWorker.Create;
begin
    inherited Create;

    m_pLock          := TQRVCLThreadLock.Create;
    m_pJobs          := TList.Create;
    m_Idle           := False;
    m_Canceled       := False;
    m_pProcessingJob := nil;
    m_fOnProcess     := nil;
    m_fOnDone        := nil;
    m_fOnCanceled    := nil;
    m_Started        := False;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRVCLThreadWorker.Destroy;
begin
    // break the thread execution
    Terminate;

    if (m_Started) then
        // wait until worker has really stopped to work
        WaitFor;

    m_pJobs.Free;
    m_pLock.Free;

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetProcessingJob: TQRThreadJob;
begin
    m_pLock.Lock;
    Result := m_pProcessingJob;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnProcess: TQRThreadJobProcessEvent;
begin
    m_pLock.Lock;
    Result := m_fOnProcess;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnProcess(fOnProcess: TQRThreadJobProcessEvent);
begin
    m_pLock.Lock;
    m_fOnProcess := fOnProcess;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnDone: TQRThreadJobDoneEvent;
begin
    m_pLock.Lock;
    Result := m_fOnDone;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnDone(fOnDone: TQRThreadJobDoneEvent);
begin
    m_pLock.Lock;
    m_fOnDone := fOnDone;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnCanceled: TQRThreadJobCancelEvent;
begin
    m_pLock.Lock;
    Result := m_fOnCanceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnCanceled(fOnCanceled: TQRThreadJobCancelEvent);
begin
    m_pLock.Lock;
    m_fOnCanceled := fOnCanceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnIdle: TQRThreadJobIdleEvent;
begin
    m_pLock.Lock;
    Result := m_fOnIdle;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnIdle(fOnIdle: TQRThreadJobIdleEvent);
begin
    m_pLock.Lock;
    m_fOnIdle := fOnIdle;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.Execute;
var
    pProcessingJob: TQRThreadJob;
    count:          NativeUInt;
    idle, success:  Boolean;
    fOnIdle:        TQRThreadJobIdleEvent;
begin
    m_Started := True;

    // repeat thread execution until terminated
    while (not Terminated) do
    begin
        m_pLock.Lock;
        m_IsIdle := False;
        idle     := m_Idle;
        m_pLock.Unlock;

        // is thread idle?
        if (idle) then
        begin
            m_pLock.Lock;
            m_IsIdle := True;
            fOnIdle  := m_fOnIdle;
            m_pLock.Unlock;

            // is idle event defined?
            if (not Assigned(fOnIdle)) then
                // wait 10 ms to not overload the processor
                Sleep(10)
            else
                // notify that job is idle
                Synchronize(OnIdleNotify);

            continue;
        end;

        // get job count
        m_pLock.Lock;
        count := m_pJobs.Count;
        m_pLock.Unlock;

        // no job to process for now?
        if (count = 0) then
        begin
            // wait 10ms to not overload the processor
            Sleep(10);
            continue;
        end;

        // break the loop if worker was canceled
        if (IsCanceled) then
            break;

        m_pLock.Lock;

        try
            // get job to process
            m_pProcessingJob := m_pJobs[0];

            // job is get and will be processed, can delete it from list
            m_pJobs.Delete(0);

            pProcessingJob := m_pProcessingJob;
        finally
            m_pLock.Unlock;
        end;

        // break the loop if worker was canceled
        if (IsCanceled) then
            break;

        // skip canceled job
        if (pProcessingJob.GetStatus = EQR_JS_Canceled) then
        begin
            m_pLock.Lock;
            m_pProcessingJob := nil;
            m_pLock.Unlock;

            continue;
        end;

        // set next job status to processing
        pProcessingJob.SetStatus(EQR_JS_Processing);

        // notify that job is about to be processed
        Synchronize(OnProcessNotify);

        // process next job
        success := pProcessingJob.Process;

        // break the loop if worker was canceled
        if (IsCanceled) then
            break;

        // skip canceled job
        if (pProcessingJob.GetStatus = EQR_JS_Canceled) then
        begin
            m_pLock.Lock;
            m_pProcessingJob := nil;
            m_pLock.Unlock;

            continue;
        end;

        // set job status
        if (success) then
            pProcessingJob.SetStatus(EQR_JS_Done)
        else
            pProcessingJob.SetStatus(EQR_JS_Error);

        // notify that job is done
        Synchronize(OnDoneNotify);

        m_pLock.Lock;
        m_pProcessingJob := nil;
        m_pLock.Unlock;
    end;

    m_pLock.Lock;
    m_pProcessingJob := nil;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnProcessNotify;
var
    pJob:       TQRThreadJob;
    fOnProcess: TQRThreadJobProcessEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    pJob       := m_pProcessingJob;
    fOnProcess := m_fOnProcess;
    m_pLock.Unlock;

    // break the loop if worker was canceled
    if (IsCanceled) then
        Exit;

    // notify that job is about to be processed
    if (Assigned(fOnProcess)) then
        fOnProcess(pJob);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnDoneNotify;
var
    pJob:    TQRThreadJob;
    fOnDone: TQRThreadJobDoneEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    pJob    := m_pProcessingJob;
    fOnDone := m_fOnDone;
    m_pLock.Unlock;

    // break the loop if worker was canceled
    if (IsCanceled) then
        Exit;

    // notify that job is done
    if (Assigned(fOnDone)) then
        fOnDone(pJob);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnCanceledNotify;
var
    pJob:        TQRThreadJob;
    fOnCanceled: TQRThreadJobCancelEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    pJob        := m_pProcessingJob;
    fOnCanceled := m_fOnCanceled;
    m_pLock.Unlock;

    // notify that job is canceled
    if (Assigned(fOnCanceled)) then
        fOnCanceled(pJob);
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnIdleNotify;
var
    fOnIdle: TQRThreadJobIdleEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock;
    fOnIdle := m_fOnIdle;
    m_pLock.Unlock;

    // notify that job is canceled
    if (Assigned(fOnIdle)) then
        fOnIdle;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.AddJob(pJob: TQRThreadJob);
var
    count, i: NativeUInt;
    found:    Boolean;
begin
    m_pLock.Lock;

    try
        found := false;

        // get job count
        count := m_pJobs.Count;

        // iterate through jobs and check if job was already added
        if (count > 0) then
            for i := 0 to count - 1 do
                if (m_pJobs[i] = pJob) then
                begin
                    found := True;
                    break;
                end;

        // add job to job list
        if (not found) then
        begin
            pJob.SetStatus(EQR_JS_NotStarted);
            m_pJobs.Add(pJob);
        end;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.DeleteJob(pJob: TQRThreadJob; doCancel: Boolean);
var
    count, i: NativeUInt;
begin
    // no job to cancel?
    if (not Assigned(pJob)) then
        Exit;

    // do cancel job?
    if (doCancel) then
    begin
        // cancel processing job, if any
        pJob.Cancel;
        pJob.SetStatus(EQR_JS_Canceled);
    end;

    m_pLock.Lock;

    try
        // get job count
        count := m_pJobs.Count;

        // iterate through jobs
        if (count > 0) then
            for i := 0 to count - 1 do
                // found job to delete?
                if (m_pJobs[i] = pJob) then
                begin
                    // delete job
                    m_pJobs.Delete(i);
                    break;
                end;
    finally
        m_pLock.Unlock;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.MakeIdle(value: Boolean);
begin
    m_pLock.Lock;
    m_Idle := value;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.Cancel;
var
    count, i: NativeUInt;
    running:  Boolean;
begin
    running := False;

    m_pLock.Lock;

    try
        m_Canceled := true;

        // cancel processing job, if any
        if (Assigned(m_pProcessingJob)) then
        begin
            m_pProcessingJob.Cancel;
            m_pProcessingJob.SetStatus(EQR_JS_Canceled);
            running := True;
        end;
    finally
        m_pLock.Unlock;
    end;

    if (m_Started and running) then
    begin
        // wait until worker has really stopped to work
        Terminate;
        WaitFor;
    end;

    // notify that job is canceled
    Synchronize(OnCanceledNotify);

    // get job count
    m_pLock.Lock;
    count := m_pJobs.Count;
    m_pLock.Unlock;

    // iterate through jobs to cancel
    if (count > 0) then
        for i := 0 to count - 1 do
        begin
            // get next job to cancel
            m_pLock.Lock;
            m_pProcessingJob := m_pJobs[i];
            m_pProcessingJob.SetStatus(EQR_JS_Canceled);
            m_pLock.Unlock;

            // notify that job is canceled
            Synchronize(OnCanceledNotify);
        end;

    // clear local values
    m_pLock.Lock;
    m_pJobs.Clear;
    m_pProcessingJob := nil;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------
function TQRVCLThreadWorker.IsCanceled: Boolean;
begin
    m_pLock.Lock;
    Result := m_Canceled;
    m_pLock.Unlock;
end;
//--------------------------------------------------------------------------------------------------

end.
