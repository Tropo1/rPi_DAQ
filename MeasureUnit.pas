unit MeasureUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Console,
  GlobalTypes,
  GlobalConfig,
  GlobalConst,
  DAQ_Globals,
  Platform,
  SysUtils,
  DateUtils,
  Threads,
  SPI,
  I2C,
  GPIO,
  Serial,
  ADS8320,
  BMP280,
  SampleBufferUnit,         // Handles the Sample Buffer
  DataFileHandlingUnit;     // Handels creation and writing of samples to files


var
 CurrentCount_Transfer    : QWord;


function WriteData2Disk(Parameter:Pointer):PtrInt;
procedure WriteData2Buffer(SampleRate:LongWord;BMPSampleRate:LongWord;ADCDevice:PSPIDevice;SampleBytes:PSPIBytes);
function WriteData2Buffer_BMP_Thread(Parameter:Pointer):PtrInt;
implementation

function WriteData2Disk(Parameter:Pointer):PtrInt;

 var
 Handle: TWindowHandle;

 fname, BMPfname, PPSfname, BMP_PPSfname               : String;
 f, BMPf, PPSf, BMP_PPSf                               : File;
 DT_file, BMP_DT_file, PPSDT_file, BMP_PPSDT_file      : TDateTime;
 files_per_day                                         : Double;

 CurrentBuffer, BMPCurrentBuffer                       : Integer;
 ADC_Buffers_cleared,BMP_Buffers_cleared               : Boolean;

 i                       : Integer;
 SamplesCount            : Integer;
 FullSamplesCount        : Integer;
 startup,BMPstartup      : Boolean;

 AllSamplesFromBuffer    : TSampleArray;
 BMPAllSamplesFromBuffer : TBMPSampleArray;
 PPS                     : TPPSArray;
 BMP_PPS                 : TBMPPPSArray;

 BMP_FullSamplesCount    : Integer;


 Message                 : TMessage;

 printMaxOffset,printMinOffset : LongWord;

 begin

 Result:=0;
 Handle:=RightWindow;
 printMaxOffset:=0;
 printMinOffset:=0;
  {FillChar(PPS, SizeOf(TPPSArray), 0);
 FillChar(AllSamplesFromBuffer, SizeOf(TSampleArray), 0);
 FillChar(BMP_PPS, SizeOf(TBMPPPSArray), 0);
 FillChar(BMPAllSamplesFromBuffer, SizeOf(TBMPSampleArray), 0);}
 ConsoleWindowWriteLn(Handle,'WriteThread running...');
 CurrentBuffer:=1;
 BMPCurrentBuffer:=1;
 startup:=True;
 BMPstartup:=True;
 while not(STOP_SWITCH) do
 //if READY_SWITCH then
 //begin
  begin
   ADC_Buffers_cleared:=False;
   BMP_Buffers_cleared:=False;
   if MaxOffset<>printMaxOffset then
   begin
    ConsoleWindowWriteLn(Handle,'MaxOffSet = '+IntToStr(MaxOffset));
    printMaxOffset:=MaxOffset;
   end;
   if MinOffset<>printMinOffset then
   begin
    ConsoleWindowWriteLn(Handle,'MinOffSet = '+IntToStr(MinOffset));
    printMinOffset:=MinOffset;
   end;
   // Check if the current Buffer is full
   if SampleBuffer[CurrentBuffer]^.State=4 then
    begin

     SampleBuffer[CurrentBuffer]^.State:=2; //set state to reading

     ConsoleWindowWriteLn(Handle,'samples from buffer read');
     if startup=True then
      begin
       files_per_day:=24*60/NEW_FILE_RATE;
       fname:=GenerateFileName(SampleBuffer[CurrentBuffer]^.StartTime,'ADC\ADC_');
       Assign(f,fname);
       Rewrite(f,SizeOf(TSample));

       DT_file:=Trunc(SampleBuffer[CurrentBuffer]^.StartTime*files_per_day)/files_per_day;
       ConsoleWindowWriteLn(Handle,'start_file:'+GenerateFileName(DT_file,'ADC\ADC_'));
       startup:=False;

       PPSDT_file:=DT_file;
       PPSfname:=GenerateFileName(SampleBuffer[CurrentBuffer]^.StartTime,'ADC\ADC_PPS_');
       Assign(PPSf,PPSfname);
       Rewrite(PPSf,SizeOf(TPPSSample));
      end;
     {ConsoleWindowWriteLn(Handle,'reading PPS samples');
     PPS:= SampleBuffer[CurrentBuffer]^.PPS;
     ConsoleWindowWriteLn(Handle,'reading ADC samples');
     AllSamplesFromBuffer:=BufferReadAll(SampleBuffer[CurrentBuffer]); //empty current Buffer
     HandleFile('ADC','Data',NEW_FILE_RATE, SampleBuffer[CurrentBuffer]^.StartTime,'', f, DT_file, fname);
     BlockWrite(f, AllSamplesFromBuffer, SAMPLE_COUNT);
     HandleFile('ADC','PPS',NEW_FILE_RATE, SampleBuffer[CurrentBuffer]^.StartTime,'ADC_', PPSf, PPSDT_file, PPSfname);
     BlockWrite(PPSf, PPS, SAMPLE_COUNT);}
     WriteBuffer2Disk(SampleBuffer[CurrentBuffer],NEW_FILE_RATE, '','ADC_',f,PPSf,fname,PPSfname,DT_file);
     ConsoleWindowWriteLn(Handle,' ------ maxDT='+ IntToStr(SampleBuffer[CurrentBuffer]^.maxDT) +' ;   minDT='+ IntToStr(SampleBuffer[CurrentBuffer]^.minDT));

     // switch to next buffer
     inc(CurrentBuffer);
     CurrentBuffer:=CurrentBuffer mod N_BUFFER;
     if CurrentBuffer=0 then CurrentBuffer:= N_BUFFER;

     if SampleBuffer[CurrentBuffer]^.State <> 4 then ADC_Buffers_cleared:=True
     else
     begin
      FullSamplesCount:=0;
      for i:=1 to N_BUFFER do
       begin
        if SampleBuffer[i]^.State=4 then inc(FullSamplesCount);
       end;
      if FullSamplesCount=N_BUFFER then BUFFER_OverFlow:=True;
      //ConsoleWindowWriteLn(Handle,' BMP: ------ '+IntToStr(BMP_FullSamplesCount)+' ------');
     end;

     //STOP_SWITCH:=True;
    end
   else ADC_Buffers_cleared:=True;


    if BMPSampleBuffer[BMPCurrentBuffer]^.State=4 then
    begin
     BMPSampleBuffer[BMPCurrentBuffer]^.State:=2; //set state to reading
     ConsoleWindowWriteLn(Handle,'reading samples');

     //BMP_PPS:= BMPSampleBuffer[BMPCurrentBuffer]^.PPS;
     //BMPAllSamplesFromBuffer:=BMP_BufferReadAll(BMPSampleBuffer[BMPCurrentBuffer]); //empty current Buffer
     //ConsoleWindowWriteLn(Handle,'samples from buffer read');
     if BMPstartup=True then
      begin
       files_per_day:=24*60/NEW_FILE_RATE;
       BMPfname:=GenerateFileName(BMPSampleBuffer[BMPCurrentBuffer]^.StartTime,'BMP\BMP_');
       Assign(BMPf,BMPfname);
       Rewrite(BMPf,SizeOf(TBMPSample));
       BMP_DT_file:=Trunc(BMPSampleBuffer[BMPCurrentBuffer]^.StartTime*files_per_day)/files_per_day;
       ConsoleWindowWriteLn(Handle,'start_file:'+GenerateFileName(BMP_DT_file,'BMP\BMP_'));
       BMPstartup:=False;

       BMP_PPSDT_file:=BMP_DT_file;
       BMP_PPSfname:=GenerateFileName(BMPSampleBuffer[BMPCurrentBuffer]^.StartTime,'BMP\BMP_PPS_');
       Assign(BMP_PPSf,BMP_PPSfname);
       Rewrite(BMP_PPSf,SizeOf(TPPSSample));
      end;
     {HandleFile('BMP','Data',NEW_FILE_RATE, BMPSampleBuffer[BMPCurrentBuffer]^.StartTime,'BMP_', BMPf, BMP_DT_file, BMPfname);
     BlockWrite(BMPf, BMPAllSamplesFromBuffer, BMP_SAMPLE_COUNT);
     HandleFile('BMP','PPS',NEW_FILE_RATE, SampleBuffer[CurrentBuffer]^.StartTime,'BMP_', BMP_PPSf, BMP_PPSDT_file, BMP_PPSfname);
     BlockWrite(BMP_PPSf, BMP_PPS, BMP_SAMPLE_COUNT); }
     WriteBMPBuffer2Disk(BMPSampleBuffer[BMPCurrentBuffer],NEW_FILE_RATE, '','BMP_',BMPf,BMP_PPSf,BMPfname,BMP_PPSfname,BMP_DT_file);

     ConsoleWindowWriteLn(Handle,'BMP: ------ maxDT='+ IntToStr(BMPSampleBuffer[BMPCurrentBuffer]^.maxDT) +' ;   minDT='+ IntToStr(BMPSampleBuffer[BMPCurrentBuffer]^.minDT));
     ConsoleWindowWriteLn(Handle,'PPS_count = '+IntToStr(PPS_count));
     // switch to next buffer
     inc(BMPCurrentBuffer);
     BMPCurrentBuffer:=BMPCurrentBuffer mod BMP_N_BUFFER;
     if BMPCurrentBuffer=0 then BMPCurrentBuffer:= BMP_N_BUFFER;

     if BMPSampleBuffer[BMPCurrentBuffer]^.State <> 4 then BMP_Buffers_cleared:=True
     else
     begin
      BMP_FullSamplesCount:=0;
      for i:=1 to BMP_N_BUFFER do
       begin
        if BMPSampleBuffer[i]^.State=4 then inc(BMP_FullSamplesCount);
       end;
      if BMP_FullSamplesCount=BMP_N_BUFFER then BMP_BUFFER_OverFlow:=True;
      //ConsoleWindowWriteLn(Handle,' BMP: ------ '+IntToStr(BMP_FullSamplesCount)+' ------');
     end;
     //STOP_SWITCH:=True;
    end
    else BMP_Buffers_cleared:=True;

    //In case the next buffer from ADC or BMP is not full, we wait for Message to signal it's filled up
    if ADC_Buffers_cleared and BMP_Buffers_cleared then ThreadReceiveMessage(Message);

  end;
  ConsoleWindowWriteLn(Handle,' ------ all done ------');
  //end;
 end;

procedure WriteData2Buffer(SampleRate:LongWord;BMPSampleRate:LongWord;ADCDevice:PSPIDevice;SampleBytes:PSPIBytes);
{This is the procedure that runs alone on a dedicated thread without anything else on the core.
It takes a sample at the times correspodning to the sample rate and writes the sample to one of the buffers,
cycling through the buffers}
var
 CurrentCount, C1     : QWord;
 LastCount            : QWord;
 trueLastCount        : QWord;
 CurrentBuffer        : Word;
 SampleTime           : LongWord;
 startSwitch          : Boolean;
 Sample               : Word;
 dt                   : Word;
 dt2                  : Byte;
 dt_samples           : Byte;
 BMPSampleTime        : LongWord;
 BMPLastCount_Trig    : QWord;
 BMPLastCount         : QWord;
 BMPtrueLastCount     : QWord;
 clk_freq             : LongWord;

 sec_offs             : QWord;
 //Offset               : LongWord;
 BMP_Trig_Offset      : LongWord;
 BMP_Meas_Offset      : LongWord;
 PPS                  : Byte;

 PPS_count_last       : LongWord;
 PPS_time_last        : QWord;
 Message              : TMessage;

begin
 FillChar(Message,SizeOf(TMessage),0);

 clk_freq     := CLOCK_FREQUENCY;
 CLOCK_OFFSET := Trunc((ClockBase-TIME_TICKS_TO_1899)/10);

 PPS := 0;
 PPS_count_last := 0;


 MaxOffset:=0;
 MinOffset:=0;

 BMP_Trig_Offset:=0;
 BMP_Meas_Offset:=0;

 SampleTime     := clk_freq div SampleRate; // number of clock ticks between taking samples
 BMPSampleTime  := clk_freq div BMPSampleRate; // number of clock ticks between taking samples

 STOP_SWITCH   := False;
 startSwitch   := False;

 // We will start with the 1st Buffer
 CurrentBuffer := 1;
 SampleBuffer[CurrentBuffer]^.State:=3; // set state to writing
 SampleBuffer[CurrentBuffer]^.maxDT:=SampleTime;
 SampleBuffer[CurrentBuffer]^.minDT:=SampleTime;
 while True do
 begin
  if READY_SWITCH then
  begin

   {Check our tick count for elapsed time}
   CurrentCount := ClockGetTotal;

   if PPS_count_last=0 then PPS_count_last:=PPS_count;
   // waiting until we are at a full second to start measuring
   if not(startSwitch) and (PPS_count_last<PPS_count) then
   begin
    LastCount          := CurrentCount+clk_freq-SampleTime;
    trueLastCount      := CurrentCount+clk_freq;
    BMPLastCount       := CurrentCount+clk_freq-BMPSampleTime;
    BMPtrueLastCount   := CurrentCount+clk_freq;
    BMPLastCount_Trig  := CurrentCount+clk_freq-BMPSampleTime-round(BMP_MEAS_TIME*clk_freq);
    startSwitch        := True;
    PPS_time_last      := PPS_time;
    PPS_count_last     := PPS_count;
   end;

   if startSwitch then
   begin

    if CurrentCount >= (BMPLastCount_Trig + BMPSampleTime) then
      begin
        Message.Msg:=0;
        BMPLastCount_Trig:= BMPLastCount_Trig+BMPSampleTime*LongWord(Max(1,((CurrentCount-BMPLastCount_Trig) div BMPSampleTime)))+BMP_Trig_Offset;
        BMP_Trig_Offset:=0;
        ThreadSendMessage(BMPThread,Message);
      end;

    if CurrentCount >= (BMPLastCount + BMPSampleTime) then
      begin
        Message.Msg:=CurrentCount-BMPLastCount;
        Message.Time:=CurrentCount-BMPtrueLastCount;
        BMPLastCount:= BMPLastCount+BMPSampleTime*LongWord(Max(1,((CurrentCount-BMPLastCount) div BMPSampleTime)))+BMP_Meas_Offset;
        BMP_Meas_Offset:=0;
        CurrentCount_Transfer:= CurrentCount;
        ThreadSendMessage(BMPThread,Message);
      end;

    if CurrentCount >= (LastCount+SampleTime) then
    begin
     C1:=ClockGetTotal;
     Sample:=ADC_getSample(ADCDevice); //<- first thing we need to do here is to get the sample from the ADC
     dt2:=ClockGetTotal-C1;
     dt    :=CurrentCount-LastCount;
     dt_samples   :=CurrentCount-trueLastCount;
     trueLastCount:=CurrentCount;
     //Sample:=dt;

     // If there are no samples in the buffer yet, save the current Time as Start time in the buffer:
     if SampleBuffer[CurrentBuffer]^.Count=0 then SampleBuffer[CurrentBuffer]^.StartTime:=(CurrentCount+CLOCK_OFFSET)/US_PER_DAY;

     SampleBuffer[CurrentBuffer]^.maxDT:=Max(SampleBuffer[CurrentBuffer]^.maxDT,dt);
     SampleBuffer[CurrentBuffer]^.minDT:=Min(SampleBuffer[CurrentBuffer]^.minDT,dt);


     if PPS_count_last<PPS_count then
      begin
       PPS:=1;
       PPS_count_last:=PPS_count;

       sec_offs:=(PPS_time div clk_freq)-(PPS_time_last div clk_freq) -1;
       Offset:=(PPS_time mod clk_freq)-(PPS_time_last mod clk_freq) + sec_offs*clk_freq;

       Offset:=0; // <-- for bug fixing!!!

       BMP_Trig_Offset:=Offset;
       BMP_Meas_Offset:=Offset;
       PPS_time_last:=PPS_time;
       MaxOffset:=Max(MaxOffset,Offset);
       MinOffset:=Max(MaxOffset,Offset);
      end
     else
      begin
       PPS:=0;
       Offset:=0;
      end;
     //PPS:=dt2;
     if dt >= 2*SampleTime then
      begin
       SampleBuffer[CurrentBuffer]^.PPS[(SampleBuffer[CurrentBuffer]^.Count) mod SAMPLE_COUNT]:=(0 shl 7) or (dt2);
       BufferWrite(SampleBuffer[CurrentBuffer],Sample);
      end;
     SampleBuffer[CurrentBuffer]^.PPS[(SampleBuffer[CurrentBuffer]^.Count) mod SAMPLE_COUNT]:=(PPS shl 7) or (dt2);
     //Write current Sample to Buffer
     BufferWrite(SampleBuffer[CurrentBuffer],Sample);
     //BufferWrite(SampleBuffer[CurrentBuffer],dt);


     // Check if Buffer is Full, if TRUE, switch to next buffer
     if SampleBuffer[CurrentBuffer]^.State=4 then
     begin
      ThreadSendMessage(WriteThread,Message);
      inc(CurrentBuffer);
      CurrentBuffer := CurrentBuffer mod N_BUFFER;
      if CurrentBuffer=0 then CurrentBuffer:= N_BUFFER;

      // After switching, check if the "new" buffer is empty. It really should be,
      // since otherwise we sample faster than we can write to disc. If TRUE, change state to writing
      if SampleBuffer[CurrentBuffer]^.State=1 then
      begin
       SampleBuffer[CurrentBuffer]^.State:=3;

       SampleBuffer[CurrentBuffer]^.maxDT:=SampleTime;
       SampleBuffer[CurrentBuffer]^.minDT:=SampleTime;
      end;
     end;

     //LastCount:=CurrentCount;
     LastCount:=LastCount+SampleTime*LongWord(Max(1,(dt div SampleTime)))+Offset;


    end;

    {if CurrentCount > (StartCount + 1810000000) then
    begin
     STOP_SWITCH:=True;
    end;}

    if STOP_SWITCH then Break;

    end; // if startSwitch
  end; // if READY_SWITCH
 end; //while

end; //procedure


function WriteData2Buffer_BMP_Thread(Parameter:Pointer):PtrInt;
{This is the procedure that runs alone on a dedicated thread without anything else on the core.
It takes a sample at the times correspodning to the sample rate and writes the sample to one of the buffers,
cycling through the buffers}
var
 CurrentBuffer        : Word;
 SampleTime           : LongWord;
 BMPSample            : TBMPSample;
 raw_p,raw_t          : LongWord;
 Message              : TMessage;
 SampleRate           : LongWord;
 BMPDevice            : PI2CDevice;
 PPS_count_last       : LongWord;
 PPS                  : Byte;
 dt_sample            : Byte;
begin
 Result:=0;
 SampleRate:=100;
 BMPDevice:=BMPDevice_glob;
 //STOP_SWITCH   := False;

 SampleTime     := CLOCK_FREQUENCY div SampleRate; // number of clock ticks between taking samples
 // We will start with the 1st Buffer
 CurrentBuffer := 1;
 BMPSampleBuffer[CurrentBuffer]^.State:=3; // set state to writing
 BMPSampleBuffer[CurrentBuffer]^.maxDT:=SampleTime;
 BMPSampleBuffer[CurrentBuffer]^.minDT:=SampleTime;

 PPS_count_last:=0;

 while True do
 begin
 //if READY_SWITCH then
 //begin
  ThreadReceiveMessage(Message);

  if PPS_count_last=0 then PPS_count_last:=PPS_count;

  if Message.Msg=0 then
   begin
    BMP280_init_forced_meas(BMPDevice);
   end
  else
   begin
    BMP280_read_raw_data(BMPDevice,raw_t,raw_p);
    BMPSample[0]:=raw_t;
    BMPSample[1]:=raw_p;
    if BMPSampleBuffer[CurrentBuffer]^.Count=0 then BMPSampleBuffer[CurrentBuffer]^.StartTime:=(CurrentCount_Transfer+CLOCK_OFFSET)/US_PER_DAY;
    BMPSampleBuffer[CurrentBuffer]^.maxDT:=Max(BMPSampleBuffer[CurrentBuffer]^.maxDT,Message.Msg);
    BMPSampleBuffer[CurrentBuffer]^.minDT:=Min(BMPSampleBuffer[CurrentBuffer]^.minDT,Message.Msg);

    if PPS_count_last<PPS_count then
      begin
       PPS:=1;
       PPS_count_last:=PPS_count;
      end
    else
      begin
       PPS:=0;
      end;
    dt_sample:=Byte(Message.Time);
    BMPSampleBuffer[CurrentBuffer]^.PPS[(BMPSampleBuffer[CurrentBuffer]^.Count) mod BMP_SAMPLE_COUNT]:=(PPS shl 7) or dt_sample;
    BMP_BufferWrite(BMPSampleBuffer[CurrentBuffer],BMPSample);
   end;

  // Check if Buffer is Full, if TRUE, switch to next buffer
   if BMPSampleBuffer[CurrentBuffer]^.State=4 then
     begin
      ThreadSendMessage(WriteThread,Message);
      inc(CurrentBuffer);
      CurrentBuffer := CurrentBuffer mod BMP_N_BUFFER;
      if CurrentBuffer=0 then CurrentBuffer:= BMP_N_BUFFER;

      // After switching, check if the "new" buffer is empty. It really should be,
      // since otherwise we sample faster than we can write to disc. If TRUE, change state to writing
      if BMPSampleBuffer[CurrentBuffer]^.State=1 then
      begin
       BMPSampleBuffer[CurrentBuffer]^.State:=3;

       BMPSampleBuffer[CurrentBuffer]^.maxDT:=SampleTime;
       BMPSampleBuffer[CurrentBuffer]^.minDT:=SampleTime;
      end;
     end;

    if STOP_SWITCH then Break;
 end; //while
 //end;
end; //procedure


end.

