unit ThreadUnit;

{$mode objfpc}{$H+}

{ Advanced example - Dedicated CPU                                             }
{                                                                              }
{ This file contains the main functionality for our dedicated CPU example.     }
{                                                                              }

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  DAQ_Globals,
  Platform,
  Threads,
  SysUtils,
  Console,          {Include the console unit so we can output logging to the screen}
  SPI,
  I2C,
  GPIO,
  BCM2710,
  ADS8320,
  //SampleBufferUnit, {Handles the Sample Buffer (e.g. declaring the type)}
  //DataFileHandlingUnit,
  MeasureUnit;


{The start function which does all the setup work for our dedicated thread}
procedure StartDedicatedThread_orig(Handle:TWindowHandle);
procedure StartSampleThreads(Handle:TWindowHandle);

implementation


//var
// RightWindow:TWindowHandle;


{Forward declaration of our dedicated CPU thread function}
function ADC_Sampling_Execute(Parameter:Pointer):PtrInt; forward;
function BMP_Sampling_Execute(Parameter:Pointer):PtrInt; forward;
function PPS_Sampling_Execute(Parameter:Pointer):PtrInt; forward;
function StartDedicatedThread(Handle:TWindowHandle;ThreadFunction : tthreadfunc; cpu_id : Byte):TThreadHandle; forward;

{This is the startup function which creates the dedicated CPU thread and handles all of
 the setup work to migrate other threads away from the selected CPU. The comments contain
 a lot of important information, make sure you read them well}
procedure StartDedicatedThread_orig(Handle:TWindowHandle);
var
 Count:Integer;
 Message:TMessage;
 CurrentCPU:LongWord;
 DedicatedThread:TThreadHandle;
 ThreadCurrent:PThreadSnapshot;
 ThreadSnapshot:PThreadSnapshot;

 BMPDevice:PI2CDevice;
begin

 BMPDevice   := BMPDevice_glob;
 {Wait for everything to be ready after startup}
 Sleep(3000);

 {Create another console window so we can track the progress of our thread later}
 RightWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,False);

 //ConsoleWindowWriteLn(RightWindow,'SampleBuffer1^.State='+IntToStr(SampleBuffer1^.State));
 {Some initial housekeeping just to be safe, check the number of CPUs available}
 if CPUGetCount < 4 then
  begin
   {Less than 4 is bad, we can't continue with the example}
   //ConsoleWindowWriteLn(Handle,'Error, less than 4 CPUs available');
   Exit;
  end;
 //ConsoleWindowWriteLn(Handle,'CPU count is ' + IntToStr(CPUGetCount));


 {First step is to create a new thread and assign it to the CPU that we want to take
  over, this is just the same as creating any other thread except we want to explicitly
  set the CPU for it to run on and also the affinity so that it cannot run on any other CPU.

  We can do this in one step by calling the SysBeginThreadEx() function but we can also do it
  by using the normal BeginThread() function and then adjusting the CPU and affinity later}
 DedicatedThread:=BeginThread(@ADC_Sampling_Execute,nil,DedicatedThread,THREAD_STACK_DEFAULT_SIZE);
 //ConsoleWindowWriteLn(Handle,'Created dedicated CPU thread with handle ' + IntToHex(DedicatedThread,8));


 {Let's set the name of our thread so we can see it in the thread list}
 ThreadSetName(DedicatedThread,'Dedicated Sampling Thread');

 {Now we can set the affinity of our thread to CPU 3 and wait for the scheduler to migrate it for us}
 ThreadSetAffinity(DedicatedThread,CPU_AFFINITY_3);


 {Migrations happen during context switches, so our thread may not be instantly on the new CPU, instead
  we check where our thread is and wait for it to migrate if needed}
 CurrentCPU:=ThreadGetCPU(DedicatedThread);
 if CurrentCPU <> CPU_ID_3 then
  begin
   ConsoleWindowWriteLn(Handle,'Thread ' + IntToHex(DedicatedThread,8) + ' currently on ' + CPUIDToString(CurrentCPU));

   {Keep checking until it is migrated}
   while ThreadGetCPU(DedicatedThread) <> CPU_ID_3 do
    begin
     Sleep(1000);
    end;
  end;
 ConsoleWindowWriteLn(Handle,'Thread ' + IntToHex(DedicatedThread,8) + ' now on ' + CPUIDToString(ThreadGetCPU(DedicatedThread)));


 {Now we disable thread migrations temporarily so that we don't have threads moving around while we
  are trying to setup our dedicated CPU, you can see this on the "Scheduler" page in web status}
 SchedulerMigrationDisable;
 ConsoleWindowWriteLn(Handle,'Disabled scheduler migration');


 {disable thread allocation but only for that CPU and not the others}
 SchedulerAllocationDisable(CPU_ID_3);
 ConsoleWindowWriteLn(Handle,'Disabled scheduler allocation for ' + CPUIDToString(CPU_ID_3));


 {migrate all of the other threads away from our dedicated CPU. We can use the ThreadSnapshotCreate() function to get a current list}

 {We also want to count how many threads need to be migrated so we'll start with zero}
 Count:=0;


 {Then create a thread snapshot, the snapshot contains all of the thread information at a precise
  point in time. The real thread information changes hundreds of times per second and so isn't easy
  to read directly}
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin

   {Get the first thread in the snapshot}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin

     {Check the handle of the thread to make sure it is not our dedicated CPU thread}
     if ThreadCurrent^.Handle <> DedicatedThread then
      begin

       {Check the CPU to see if it is on CPU 3}
       if ThreadCurrent^.CPU = CPU_ID_3 then
        begin

         {Check for one of the special threads and if it is not then ask it to migrate}
         if ThreadCurrent^.Handle = SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_IDLE) then
          begin

           {This is the idle thread, we can't migrate this one}
           ConsoleWindowWriteLn(Handle,'Skipping migration of idle thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_IRQ) then
          begin

           {This one is the IRQ thread and it can't be migrated either}
           ConsoleWindowWriteLn(Handle,'Skipping migration of IRQ thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_FIQ) then
          begin

           {FIQ threads also can't be migrated but they never run so it doesn't matter}
           ConsoleWindowWriteLn(Handle,'Skipping migration of FIQ thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_SWI) then
          begin

           {And the SWI threads are the same so we can ignore them as well}
           ConsoleWindowWriteLn(Handle,'Skipping migration of SWI thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else
          begin

           {If the thread is not any of those then it must be a normal thread. Ask the scheduler to migrate it
            to CPU 0 instead, we could specify any CPU}
           ThreadSetCPU(ThreadCurrent^.Handle,CPU_ID_0);
           ConsoleWindowWriteLn(Handle,'Migrating thread "' + ThreadGetName(ThreadCurrent^.Handle) + '" to ' + CPUIDToString(CPU_ID_0));

           {Add one to our migrated thread count}
           Inc(Count);
          end;
        end;
      end
     else
      begin

       {No need to migrate our own thread, that wouldn't make any sense!}
       ConsoleWindowWriteLn(Handle,'Skipping migration for "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
      end;

     {Get the next thread from the snapshot}
     ThreadCurrent:=ThreadCurrent^.Next;
    end;

   {Remember to destroy the snapshot when we have finished using it}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end;

 {Print the number of threads that we asked to migrate}
 ConsoleWindowWriteLn(Handle,'Migrated ' + IntToStr(Count) +  ' threads from ' + CPUIDToString(CPU_ID_3));

 {Thread migrations only happen during context switches.
  Sleep for a second and then quickly run through a new snapshot to check if everyone has migrated}
 Sleep(1000);
 {Create the snapshot and reset the count}
 Count:=0;
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin
   {Get the first thread}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin
     {Check the handle and the CPU}
     if (ThreadCurrent^.Handle <> DedicatedThread) and (ThreadCurrent^.CPU = CPU_ID_3) then
      begin
       if (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_IDLE))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_IRQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_FIQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(CPU_ID_3,THREAD_TYPE_SWI)) then
        begin
         {Add one to our count}
         Inc(Count);
        end;
      end;

     {Get the next thread}
     ThreadCurrent:=ThreadCurrent^.Next;
    end;

   {Destroy the snapshot}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end;

 {Check the count to see if any threads have not migrated yet, we won't proceed if there are any.}
 if Count <> 0 then
  begin
   ConsoleWindowWriteLn(Handle,'Error, ' + IntToStr(Count) +  ' threads remaining on ' + CPUIDToString(CPU_ID_3));
   Exit;
  end;
 ConsoleWindowWriteLn(Handle,'No threads remaining on ' + CPUIDToString(CPU_ID_3) + ' proceeding with example');

 {Send a message to our dedicated CPU thread to tell it we are done and it can go ahead}
 FillChar(Message,SizeOf(TMessage),0);
 ThreadSendMessage(DedicatedThread,Message);
 ConsoleWindowWriteLn(Handle,'Sent a message to the dedicated CPU thread');

 {Enable thread migrations now that we are all done, the scheduler will not touch our dedicated CPU}
 SchedulerMigrationEnable;
 ConsoleWindowWriteLn(Handle,'Enabled scheduler migration');

 READY_SWITCH:=True;
 WriteData2Disk(nil);


end;

function StartDedicatedThread(Handle:TWindowHandle;ThreadFunction : tthreadfunc; cpu_id : Byte):TThreadHandle;
var
 Count:Integer;
 Message:TMessage;
 CurrentCPU:LongWord;
 DedicatedThread:TThreadHandle;
 ThreadCurrent:PThreadSnapshot;
 ThreadSnapshot:PThreadSnapshot;
 MigrationComplete:Boolean;
 n_migration:Integer;
begin


 //ConsoleWindowWriteLn(RightWindow,'SampleBuffer1^.State='+IntToStr(SampleBuffer1^.State));
 {Some initial housekeeping just to be safe, check the number of CPUs available}
 if CPUGetCount < 4 then 
  begin
   {Less than 4 is bad, we can't continue with the example}
   //ConsoleWindowWriteLn(Handle,'Error, less than 4 CPUs available');
   Exit;
  end;
 //ConsoleWindowWriteLn(Handle,'CPU count is ' + IntToStr(CPUGetCount));
 
 
 {First step is to create a new thread and assign it to the CPU that we want to take
  over, this is just the same as creating any other thread except we want to explicitly
  set the CPU for it to run on and also the affinity so that it cannot run on any other CPU. 
  
  We can do this in one step by calling the SysBeginThreadEx() function but we can also do it
  by using the normal BeginThread() function and then adjusting the CPU and affinity later}
 DedicatedThread:=BeginThread(ThreadFunction,nil,DedicatedThread,THREAD_STACK_DEFAULT_SIZE);
 // Return ThreadHandle:
 Result:=DedicatedThread;

 //ConsoleWindowWriteLn(Handle,'Created dedicated CPU thread with handle ' + IntToHex(DedicatedThread,8));
 
 
 {Let's set the name of our thread so we can see it in the thread list}
 ThreadSetName(DedicatedThread,'Dedicated Sampling Thread');

 {Now we can set the affinity of our thread to CPU 3 and wait for the scheduler to migrate it for us}
 ThreadSetAffinity(DedicatedThread,(1 shl cpu_id));
 
 
 {Migrations happen during context switches, so our thread may not be instantly on the new CPU, instead
  we check where our thread is and wait for it to migrate if needed}
 CurrentCPU:=ThreadGetCPU(DedicatedThread);
 if CurrentCPU <> cpu_id then
  begin
   //ConsoleWindowWriteLn(Handle,'Thread ' + IntToHex(DedicatedThread,8) + ' currently on ' + CPUIDToString(CurrentCPU));
   
   {Keep checking until it is migrated}
   while ThreadGetCPU(DedicatedThread) <> cpu_id do
    begin
     Sleep(1000);
    end;
  end;
 //ConsoleWindowWriteLn(Handle,'Thread ' + IntToHex(DedicatedThread,8) + ' now on ' + CPUIDToString(ThreadGetCPU(DedicatedThread)));
 
 
 {Now we disable thread migrations temporarily so that we don't have threads moving around while we
  are trying to setup our dedicated CPU, you can see this on the "Scheduler" page in web status}
 SchedulerMigrationDisable;
 //ConsoleWindowWriteLn(Handle,'Disabled scheduler migration');
 
 
 {disable thread allocation but only for that CPU and not the others}
 SchedulerAllocationDisable(cpu_id);
 //ConsoleWindowWriteLn(Handle,'Disabled scheduler allocation for ' + CPUIDToString(cpu_id));
 
 
 {migrate all of the other threads away from our dedicated CPU. We can use the ThreadSnapshotCreate() function to get a current list}
 
 {We also want to count how many threads need to be migrated so we'll start with zero}
 Count:=0; 
 
 
 {Then create a thread snapshot, the snapshot contains all of the thread information at a precise 
  point in time. The real thread information changes hundreds of times per second and so isn't easy
  to read directly}
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin
  
   {Get the first thread in the snapshot}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin
    
     {Check the handle of the thread to make sure it is not our dedicated CPU thread}
     if ThreadCurrent^.Handle <> DedicatedThread then
      begin
      
       {Check the CPU to see if it is on CPU 3}
       if ThreadCurrent^.CPU = cpu_id then
        begin
         
         {Check for one of the special threads and if it is not then ask it to migrate}
         if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IDLE) then
          begin
          
           {This is the idle thread, we can't migrate this one}
           //ConsoleWindowWriteLn(Handle,'Skipping migration of idle thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IRQ) then
          begin
          
           {This one is the IRQ thread and it can't be migrated either}
           //ConsoleWindowWriteLn(Handle,'Skipping migration of IRQ thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_FIQ) then
          begin
          
           {FIQ threads also can't be migrated but they never run so it doesn't matter}
           //ConsoleWindowWriteLn(Handle,'Skipping migration of FIQ thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_SWI) then
          begin
          
           {And the SWI threads are the same so we can ignore them as well}
           //ConsoleWindowWriteLn(Handle,'Skipping migration of SWI thread "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
          end
         else
          begin
          
           {If the thread is not any of those then it must be a normal thread. Ask the scheduler to migrate it
            to CPU 0 instead, we could specify any CPU}
           ThreadSetCPU(ThreadCurrent^.Handle,CPU_ID_0);
           //ConsoleWindowWriteLn(Handle,'Migrating thread "' + ThreadGetName(ThreadCurrent^.Handle) + '" to ' + CPUIDToString(CPU_ID_0));
           //ThreadSetCPU(ThreadCurrent^.Handle,cpu_id-2);
           //ConsoleWindowWriteLn(Handle,'Migrating thread "' + ThreadGetName(ThreadCurrent^.Handle) + '" to ' + CPUIDToString(cpu_id-2));
           {Add one to our migrated thread count}
           Inc(Count);
          end;          
        end; 
      end
     else
      begin
      
       {No need to migrate our own thread, that wouldn't make any sense!}
       //ConsoleWindowWriteLn(Handle,'Skipping migration for "' + ThreadGetName(ThreadCurrent^.Handle) + '"');
      end;
     
     {Get the next thread from the snapshot}
     ThreadCurrent:=ThreadCurrent^.Next;
    end; 
   
   {Remember to destroy the snapshot when we have finished using it}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end; 
  
 {Print the number of threads that we asked to migrate}
 ConsoleWindowWriteLn(Handle,'Migrated ' + IntToStr(Count) +  ' threads from ' + CPUIDToString(cpu_id));
 MigrationComplete:=False;
 n_migration:=0;
 while not(MigrationComplete) and (n_migration<30) do
  begin
 {Thread migrations only happen during context switches.
  Sleep for a second and then quickly run through a new snapshot to check if everyone has migrated}
 Sleep(1000);
 {Create the snapshot and reset the count}
 Count:=0;
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin
   {Get the first thread}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin
     {Check the handle and the CPU}
     if (ThreadCurrent^.Handle <> DedicatedThread) and (ThreadCurrent^.CPU = cpu_id) then
      begin
       if (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IDLE))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IRQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_FIQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_SWI)) then
        begin
         {Add one to our count}
         Inc(Count);
        end;
      end;
      
     {Get the next thread}
     ThreadCurrent:=ThreadCurrent^.Next;
    end;
    
   {Destroy the snapshot}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end;
   inc(n_migration);
   if Count=0 then MigrationComplete:=True;
  end;

 {Check the count to see if any threads have not migrated yet, we won't proceed if there are any.}
 if not(MigrationComplete) then
  begin
   ConsoleWindowWriteLn(Handle,'Error, ' + IntToStr(Count) +  ' threads remaining on ' + CPUIDToString(cpu_id));
   Exit;
  end;
 ConsoleWindowWriteLn(Handle,'No threads remaining on ' + CPUIDToString(cpu_id) + ' proceeding with example');
 
 {Send a message to our dedicated CPU thread to tell it we are done and it can go ahead}
 FillChar(Message,SizeOf(TMessage),0);
 ThreadSendMessage(DedicatedThread,Message);
 //ConsoleWindowWriteLn(Handle,'Sent a message to the dedicated CPU thread');

 {Enable thread migrations now that we are all done, the scheduler will not touch our dedicated CPU}
 SchedulerMigrationEnable;
 //ConsoleWindowWriteLn(Handle,'Enabled scheduler migration');

end;

procedure StartSampleThreads(Handle:TWindowHandle);
var
 BMPDevice:PI2CDevice;
 PPSThread:TThreadHandle;
begin
 Sleep(3000);
 READY_SWITCH:=False;
 {Create another console window so we can track the progress of our thread later}
 RightWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,False);

 StartDedicatedThread(Handle,@ADC_Sampling_Execute,CPU_ID_3);
 StartDedicatedThread(Handle,@BMP_Sampling_Execute,CPU_ID_2);

 PPSThread:=BeginThread(@PPS_Sampling_Execute,nil,PPSThread,THREAD_STACK_DEFAULT_SIZE);
 WriteThread:=BeginThread(@WriteData2Disk,nil,WriteThread,THREAD_STACK_DEFAULT_SIZE);
end;

{This is the thread function for our dedicated CPU thread.}
function ADC_Sampling_Execute(Parameter:Pointer):PtrInt;
var

 Message:TMessage;
 ADCDevice:PSPIDevice;
 SampleBytes:PSPIBytes;

 //writingSampleBuffer:PSampleBuffer;
begin
 ADCDevice   := ADCDevice_glob;
 SampleBytes := SampleBytes_glob;

 Result:=0;
 {Do a loop while we are not on our dedicated CPU}
 ConsoleWindowWriteLn(RightWindow,'Waiting for migration to ' + CPUIDToString(CPU_ID_3));
 while ThreadGetCPU(ThreadGetCurrent) <> CPU_ID_3 do
  begin
   Sleep(1000);
  end;
 {Wait for a message from the main thread to say we are ready to go}
 //ConsoleWindowWriteLn(RightWindow,'Waiting for a message from the main thread');
 ThreadReceiveMessage(Message);
 ConsoleWindowWriteLn(RightWindow,'Received a message, taking control of CPU');
 //ConsoleWindowWriteLn(RightWindow,'Disabling scheduler preemption on ' + CPUIDToString(CPU_ID_3));
 SchedulerPreemptDisable(CPU_ID_3);
 //ConsoleWindowWriteLn(RightWindow,'Disabling interrupts and fast interrupts on ' + CPUIDToString(CPU_ID_3));
 DisableFIQ;
 DisableIRQ;

 WriteData2Buffer(100000,100,ADCDevice,SampleBytes);

end;

function BMP_Sampling_Execute(Parameter:Pointer):PtrInt;
var

 Message:TMessage;
 BMPDevice:PI2CDevice;
 //writingSampleBuffer:PSampleBuffer;
begin
 BMPDevice   := BMPDevice_glob;
// SampleBytes := SampleBytes_glob;

 Result:=0;
 {Do a loop while we are not on our dedicated CPU}
 //ConsoleWindowWriteLn(RightWindow,'Waiting for migration to ' + CPUIDToString(CPU_ID_2));
 while ThreadGetCPU(ThreadGetCurrent) <> CPU_ID_2 do
  begin
   Sleep(1000);
  end;
 {Wait for a message from the main thread to say we are ready to go}
 //ConsoleWindowWriteLn(RightWindow,'Waiting for a message from the main thread');
 ThreadReceiveMessage(Message);
 ConsoleWindowWriteLn(RightWindow,'Received a message, taking control of CPU');
 //ConsoleWindowWriteLn(RightWindow,'Disabling scheduler preemption on ' + CPUIDToString(CPU_ID_2));
 SchedulerPreemptDisable(CPU_ID_2);
 //ConsoleWindowWriteLn(RightWindow,'Disabling interrupts and fast interrupts on ' + CPUIDToString(CPU_ID_2));
 //DisableFIQ;
 //DisableIRQ;

 //WriteData2Buffer_BMP(100,BMPDevice);
 BMPThread:=BeginThread(@WriteData2Buffer_BMP_Thread,nil,BMPThread,THREAD_STACK_DEFAULT_SIZE);
end;

procedure Inc_PPS_count(Parmeter:Pointer;Pin,Trigger:LongWord);
begin
 PPS_time:=ClockGetTotal;
 Inc(PPS_count);
end;

procedure Get_PPS_Time(Parmeter:Pointer;Pin,Trigger:LongWord);
begin
 PPS_time:=ClockGetTotal;
end;

function PPS_Sampling_Execute(Parameter:Pointer):PtrInt;
var

 Message:TMessage;
 GPIODevice:PGPIODevice;

 Res,Res2: LongWord;
 //writingSampleBuffer:PSampleBuffer;
begin
 GPIODevice   := GPIODeviceGetDefault;
 PPS_count:=0;
 PPS_time:=0;
// SampleBytes := SampleBytes_glob;

 Result:=0;

 GPIODeviceStart(GPIODevice);
 //GPIODevicePullSelect(GPIODevice,GPIO_PIN_17,GPIO_PULL_DOWN);
 //GPIODeviceFunctionSelect(GPIODevice,GPIO_PIN_17,GPIO_FUNCTION_IN);

 GPIODeviceInputEvent(GPIODevice, GPIO_PIN_17, GPIO_TRIGGER_RISING, GPIO_EVENT_FLAG_INTERRUPT or GPIO_EVENT_FLAG_REPEAT, INFINITE, @Inc_PPS_count, nil);
 while PPS_time=0 do Sleep(1);

 READY_SWITCH:=True;
 for Res:=1 to 10 do
  begin
   ConsoleWindowWriteLn(LeftWindow,IntToStr(PPS_time)+'  '+IntToStr(PPS_count)+'  SB1: '+IntToStr(SampleBuffer[1]^.State)+'  '+IntToStr(SampleBuffer[1]^.Count)+'  SB2: '+IntToStr(SampleBuffer[2]^.State)+'  '+IntToStr(SampleBuffer[2]^.Count));
   Sleep(1001);
  end;

end;



end.
