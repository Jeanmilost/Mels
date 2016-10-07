{**************************************************************************************************
 * ==> UTQRThreading -----------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : This module provides the basic tools to create threaded jobs to send to a worker *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit UTQRThreading;

interface

uses System.Classes,
     System.SysUtils,
     System.SyncObjs,
     Winapi.Windows;

type
    {**
    * Job status enumeration
    *}
    EQRThreadJobStatus =
    (
        EQR_JS_Unknown = 0, // job was still not added
        EQR_JS_NotStarted,  // job was added to job list, but still not started
        EQR_JS_Processing,  // job was started
        EQR_JS_Done,        // job is done
        EQR_JS_Canceled,    // job was canceled
        EQR_JS_Error        // job is done but an error occurred
    );

    {**
    * Thread job helper
    *}
    TQRThreadJobHelper = record
        {**
        * Converts job status enumeration to string
        *@param status - job status to convert
        *@return job status as string
        *}
        class function JobStatusToStr(status: EQRThreadJobStatus): UnicodeString; static;
    end;

    {**
    * Basic interface for threaded jobs (NOTE using class instead of interface to avoid to use the
    * reference counting. Here an interface is just a basic contract that certify that several
    * functions should be implemented and to access them in a generic way)
    *@author Jean-Milost Reymond
    *}
    TQRThreadJob = class
        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Processes the job
            *@returns true on success, otherwise false
            *}
            function Process(): Boolean; virtual; abstract;

            {**
            * Cancels the job
            *}
            procedure Cancel(); virtual; abstract;

            {**
            * Gets job status
            *@return job status
            *}
            function GetStatus(): EQRThreadJobStatus; virtual; abstract;

            {**
            * Sets job status
            *@param status - job status
            *}
            procedure SetStatus(status: EQRThreadJobStatus); virtual; abstract;
    end;

    {**
    * Thread lock interface (NOTE using class instead of interface to avoid to use the reference
    * counting. Here an interface is just a basic contract that certify that several functions
    * should be implemented and to access them in a generic way)
    *@author Jean-Milost Reymond
    *}
    TQRThreadLock = class
        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Locks code block to prevent access by other threads
            *}
            procedure Lock(); virtual; abstract;

            {**
            * Unlocks code block to allow access by other threads
            *}
            procedure Unlock(); virtual; abstract;
    end;

    {**
    * Multi threading lock using Windows features
    *@author Jean-Milost Reymond
    *}
    TQRWinThreadLock = class(TQRThreadLock)
        private
            m_pCritical: RTL_CRITICAL_SECTION;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Locks code block to prevent access by other threads
            *}
            procedure Lock(); override;

            {**
            * Unlocks code block to allow access by other threads
            *}
            procedure Unlock(); override;
    end;

    {**
    * Multi threading lock using VCL features
    *@author Jean-Milost Reymond
    *}
    TQRVCLThreadLock = class(TQRThreadLock)
        private
            m_pCritical: TCriticalSection;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Locks code block to prevent access by other threads
            *}
            procedure Lock(); override;

            {**
            * Unlocks code block to allow access by other threads
            *}
            procedure Unlock(); override;
    end;

    {**
    * VCL thread worker job, processes a job in a worker thread using VCL
    *@author Jean-Milost Reymond
    *}
    TQRVCLThreadWorkerJob = class(TQRThreadJob)
        private
            m_Status: EQRThreadJobStatus;

        protected
            m_pLock: TQRVCLThreadLock;

        public
            { Construction/Destruction }
            constructor Create();  override;
            destructor  Destroy(); override;

            {**
            * Gets job status
            *@return job status
            *}
            function GetStatus(): EQRThreadJobStatus; override;

            {**
            * Sets job status
            *@param status - job status
            *}
            procedure SetStatus(status: EQRThreadJobStatus); override;
    end;

    {**
    * Called when a job is about to be processed
    *@param pJob - job to process
    *}
    TQRThreadJobProcessEvent = procedure(pJob: TQRThreadJob) of object;

    {**
    * Called when a job is done
    *@param pJob - done job
    *}
    TQRThreadJobDoneEvent = procedure(pJob: TQRThreadJob) of object;

    {**
    * Called when a job is canceled
    *@param pJob - canceled job
    *}
    TQRThreadJobCancelEvent = procedure(pJob: TQRThreadJob) of object;

    {**
    * Called when a job is idle
    *}
    TQRThreadJobIdleEvent = procedure() of object;

    {**
    * Windows thread worker, executes a list of jobs, one by one, until all jobs are processed
    *@author Jean-Milost Reymond
    *}
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

        protected
            {**
            * Gets currently processing job
            *@return currently processing job, nil if no job is currently processing
            *}
            function GetProcessingJob(): TQRThreadJob; virtual;

            {**
            * Gets OnProcess callback
            *@return OnProcess callback
            *}
            function GetOnProcess(): TQRThreadJobProcessEvent; virtual;

            {**
            * Sets OnProcess callback
            *@param fOnProcess - OnProcess callback
            *}
            procedure SetOnProcess(fOnProcess: TQRThreadJobProcessEvent); virtual;

            {**
            * Gets OnDone callback
            *@return OnDone callback
            *}
            function GetOnDone(): TQRThreadJobDoneEvent; virtual;

            {**
            * Sets OnDone callback
            *@param fOnDone- OnDone callback
            *}
            procedure SetOnDone(fOnDone: TQRThreadJobDoneEvent); virtual;

            {**
            * Gets OnCanceled callback
            *@return OnCanceled callback
            *}
            function GetOnCanceled(): TQRThreadJobCancelEvent; virtual;

            {**
            * Sets OnCanceled callback
            *@param OnCanceled - OnCanceled callback
            *}
            procedure SetOnCanceled(fOnCanceled: TQRThreadJobCancelEvent); virtual;

            {**
            * Gets OnIdle callback
            *@return OnIdle callback
            *}
            function GetOnIdle(): TQRThreadJobIdleEvent; virtual;

            {**
            * Sets OnIdle callback
            *@param OnIdle - OnCanceled callback
            *}
            procedure SetOnIdle(fOnIDle: TQRThreadJobIdleEvent); virtual;

            {**
            * Executes work
            *}
            procedure Execute(); override;

            {**
            * Notify that job is about to be processed
            *@note This function is executed on the calling thread side
            *}
            procedure OnProcessNotify(); virtual;

            {**
            * Notify that job is done
            *@note This function is executed on the calling thread side
            *}
            procedure OnDoneNotify(); virtual;

            {**
            * Notify that job is canceled
            *@note This function is executed on the calling thread side
            *}
            procedure OnCanceledNotify(); virtual;

            {**
            * Notify that worker is idle
            *@note This function is executed on the calling thread side
            *}
            procedure OnIdleNotify(); virtual;

        public
            { Construction/Destruction }
            constructor Create();  virtual;
            destructor  Destroy(); override;

            {**
            * Adds job to process list
            *@param pJob - job to add
            *}
            procedure AddJob(pJob: TQRThreadJob); virtual;

            {**
            * Deletes job from process list
            *@param pJob - job to delete
            *@param doCancel - if true, job will be canceled before deleted
            *}
            procedure DeleteJob(pJob: TQRThreadJob; doCancel: Boolean = True); virtual;

            {**
            * Pauses or resumes thread activity
            *@param value - if true, thread will become idle, otherwise resume from idle state
            *}
            procedure MakeIdle(value: Boolean); virtual;

            {**
            * Cancels all the jobs
            *}
            procedure Cancel(); virtual;

            {**
            * Check if worker was canceled
            *@return true if worker was canceled, otherwise false
            *}
            function IsCanceled(): Boolean; virtual;

            { Properties }
            property ProcessingJob: TQRThreadJob             read GetProcessingJob;
            property OnProcess:     TQRThreadJobProcessEvent read GetOnProcess  write SetOnProcess;
            property OnDone:        TQRThreadJobDoneEvent    read GetOnDone     write SetOnDone;
            property OnCanceled:    TQRThreadJobCancelEvent  read GetOnCanceled write SetOnCanceled;
            property OnIdle:        TQRThreadJobIdleEvent    read GetOnIdle     write SetOnIdle;
    end;

implementation
//------------------------------------------------------------------------------
// TQRThreadJobHelper
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
// TQRThreadJob
//------------------------------------------------------------------------------
constructor TQRThreadJob.Create();
begin
    inherited Create();
end;
//------------------------------------------------------------------------------
destructor TQRThreadJob.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
// TQRThreadLock
//------------------------------------------------------------------------------
constructor TQRThreadLock.Create();
begin
    inherited Create();
end;
//------------------------------------------------------------------------------
destructor TQRThreadLock.Destroy();
begin
    inherited Destroy;
end;
//------------------------------------------------------------------------------
// TQRWinThreadLock
//------------------------------------------------------------------------------
constructor TQRWinThreadLock.Create();
begin
    inherited Create;

    InitializeCriticalSection(m_pCritical);
end;
//------------------------------------------------------------------------------
destructor TQRWinThreadLock.Destroy();
begin
    DeleteCriticalSection(m_pCritical);

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRWinThreadLock.Lock();
begin
    EnterCriticalSection(m_pCritical);
end;
//------------------------------------------------------------------------------
procedure TQRWinThreadLock.Unlock();
begin
    LeaveCriticalSection(m_pCritical);
end;
//------------------------------------------------------------------------------
// TQRVCLThreadLock
//------------------------------------------------------------------------------
constructor TQRVCLThreadLock.Create();
begin
    inherited Create;

    m_pCritical := TCriticalSection.Create;
end;
//------------------------------------------------------------------------------
destructor TQRVCLThreadLock.Destroy();
begin
    m_pCritical.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadLock.Lock();
begin
    m_pCritical.Acquire;
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadLock.Unlock();
begin
    m_pCritical.Release;
end;
//------------------------------------------------------------------------------
// TQRVCLThreadWorkerJob
//------------------------------------------------------------------------------
constructor TQRVCLThreadWorkerJob.Create();
begin
    inherited Create;

    m_Status := EQR_JS_Unknown;
    m_pLock  := TQRVCLThreadLock.Create;
end;
//------------------------------------------------------------------------------
destructor TQRVCLThreadWorkerJob.Destroy();
begin
    m_pLock.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorkerJob.GetStatus(): EQRThreadJobStatus;
var
    status: EQRThreadJobStatus;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    status := m_Status;
    m_pLock.Unlock();

    Result := status;
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorkerJob.SetStatus(status: EQRThreadJobStatus);
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    m_Status := status;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
// TQRVCLThreadWorker
//------------------------------------------------------------------------------
constructor TQRVCLThreadWorker.Create();
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
end;
//------------------------------------------------------------------------------
destructor TQRVCLThreadWorker.Destroy();
begin
    // break the thread execution
    Terminate();

    if (Started) then
        // wait until worker has really stopped to work
        WaitFor();

    m_pJobs.Free;
    m_pLock.Free;

    inherited Destroy;
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetProcessingJob(): TQRThreadJob;
begin
    m_pLock.Lock();
    Result := m_pProcessingJob;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnProcess(): TQRThreadJobProcessEvent;
begin
    m_pLock.Lock();
    Result := m_fOnProcess;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnProcess(fOnProcess: TQRThreadJobProcessEvent);
begin
    m_pLock.Lock();
    m_fOnProcess := fOnProcess;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnDone(): TQRThreadJobDoneEvent;
begin
    m_pLock.Lock();
    Result := m_fOnDone;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnDone(fOnDone: TQRThreadJobDoneEvent);
begin
    m_pLock.Lock();
    m_fOnDone := fOnDone;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnCanceled(): TQRThreadJobCancelEvent;
begin
    m_pLock.Lock();
    Result := m_fOnCanceled;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnCanceled(fOnCanceled: TQRThreadJobCancelEvent);
begin
    m_pLock.Lock();
    m_fOnCanceled := fOnCanceled;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.GetOnIdle(): TQRThreadJobIdleEvent;
begin
    m_pLock.Lock();
    Result := m_fOnIdle;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.SetOnIdle(fOnIdle: TQRThreadJobIdleEvent);
begin
    m_pLock.Lock();
    m_fOnIdle := fOnIdle;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.Execute();
var
    pProcessingJob: TQRThreadJob;
    count:          NativeUInt;
    idle, success:  Boolean;
    fOnIdle:        TQRThreadJobIdleEvent;
begin
    // repeat thread execution until terminated
    while (not Terminated) do
    begin
        m_pLock.Lock();
        m_IsIdle := False;
        idle     := m_Idle;
        m_pLock.Unlock();

        // is thread idle?
        if (idle) then
        begin
            m_pLock.Lock();
            m_IsIdle := True;
            fOnIdle  := m_fOnIdle;
            m_pLock.Unlock();

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
        m_pLock.Lock();
        count := m_pJobs.Count;
        m_pLock.Unlock();

        // no job to process for now?
        if (count = 0) then
        begin
            // wait 10ms to not overload the processor
            Sleep(10);
            continue;
        end;

        // break the loop if worker was canceled
        if (IsCanceled()) then
            break;

        m_pLock.Lock();

        try
            // get job to process
            m_pProcessingJob := m_pJobs[0];

            // job is get and will be processed, can delete it from list
            m_pJobs.Delete(0);

            pProcessingJob := m_pProcessingJob;
        finally
            m_pLock.Unlock();
        end;

        // break the loop if worker was canceled
        if (IsCanceled()) then
            break;

        // skip canceled job
        if (pProcessingJob.GetStatus() = EQR_JS_Canceled) then
        begin
            m_pLock.Lock();
            m_pProcessingJob := nil;
            m_pLock.Unlock();

            continue;
        end;

        // set next job status to processing
        pProcessingJob.SetStatus(EQR_JS_Processing);

        // notify that job is about to be processed
        Synchronize(OnProcessNotify);

        // process next job
        success := pProcessingJob.Process();

        // break the loop if worker was canceled
        if (IsCanceled()) then
            break;

        // skip canceled job
        if (pProcessingJob.GetStatus() = EQR_JS_Canceled) then
        begin
            m_pLock.Lock();
            m_pProcessingJob := nil;
            m_pLock.Unlock();

            continue;
        end;

        // set job status
        if (success) then
            pProcessingJob.SetStatus(EQR_JS_Done)
        else
            pProcessingJob.SetStatus(EQR_JS_Error);

        // notify that job is done
        Synchronize(OnDoneNotify);

        m_pLock.Lock();
        m_pProcessingJob := nil;
        m_pLock.Unlock();
    end;

    m_pLock.Lock();
    m_pProcessingJob := nil;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnProcessNotify();
var
    pJob:       TQRThreadJob;
    fOnProcess: TQRThreadJobProcessEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    pJob       := m_pProcessingJob;
    fOnProcess := m_fOnProcess;
    m_pLock.Unlock();

    // break the loop if worker was canceled
    if (IsCanceled()) then
        Exit;

    // notify that job is about to be processed
    if (Assigned(fOnProcess)) then
        fOnProcess(pJob);
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnDoneNotify();
var
    pJob:    TQRThreadJob;
    fOnDone: TQRThreadJobDoneEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    pJob    := m_pProcessingJob;
    fOnDone := m_fOnDone;
    m_pLock.Unlock();

    // break the loop if worker was canceled
    if (IsCanceled()) then
        Exit;

    // notify that job is done
    if (Assigned(fOnDone)) then
        fOnDone(pJob);
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnCanceledNotify();
var
    pJob:        TQRThreadJob;
    fOnCanceled: TQRThreadJobCancelEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    pJob        := m_pProcessingJob;
    fOnCanceled := m_fOnCanceled;
    m_pLock.Unlock();

    // notify that job is canceled
    if (Assigned(fOnCanceled)) then
        fOnCanceled(pJob);
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.OnIdleNotify();
var
    fOnIdle: TQRThreadJobIdleEvent;
begin
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock.Lock();
    fOnIdle := m_fOnIdle;
    m_pLock.Unlock();

    // notify that job is canceled
    if (Assigned(fOnIdle)) then
        fOnIdle();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.AddJob(pJob: TQRThreadJob);
var
    count, i: NativeUInt;
    found:    Boolean;
begin
    m_pLock.Lock();

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
        m_pLock.Unlock();
    end;
end;
//------------------------------------------------------------------------------
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
        pJob.Cancel();
        pJob.SetStatus(EQR_JS_Canceled);
    end;

    m_pLock.Lock();

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
        m_pLock.Unlock();
    end;
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.MakeIdle(value: Boolean);
begin
    m_pLock.Lock();
    m_Idle := value;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
procedure TQRVCLThreadWorker.Cancel();
var
    count, i: NativeUInt;
    running:  Boolean;
begin
    running := False;

    m_pLock.Lock();

    try
        m_Canceled := true;

        // cancel processing job, if any
        if (Assigned(m_pProcessingJob)) then
        begin
            m_pProcessingJob.Cancel();
            m_pProcessingJob.SetStatus(EQR_JS_Canceled);
            running := True;
        end;
    finally
        m_pLock.Unlock();
    end;

    if (Started and running) then
    begin
        // wait until worker has really stopped to work
        Terminate;
        WaitFor();
    end;

    // notify that job is canceled
    Synchronize(OnCanceledNotify);

    // get job count
    m_pLock.Lock();
    count := m_pJobs.Count;
    m_pLock.Unlock();

    // iterate through jobs to cancel
    if (count > 0) then
        for i := 0 to count - 1 do
        begin
            // get next job to cancel
            m_pLock.Lock();
            m_pProcessingJob := m_pJobs[i];
            m_pProcessingJob.SetStatus(EQR_JS_Canceled);
            m_pLock.Unlock();

            // notify that job is canceled
            Synchronize(OnCanceledNotify);
        end;

    // clear local values
    m_pLock.Lock();
    m_pJobs.Clear();
    m_pProcessingJob := nil;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------
function TQRVCLThreadWorker.IsCanceled(): Boolean;
begin
    m_pLock.Lock();
    Result := m_Canceled;
    m_pLock.Unlock();
end;
//------------------------------------------------------------------------------

end.
