unit DAQ_Setup;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Platform,
  GlobalConst,
  GlobalConfig,
  GlobalTypes,
  //RaspberryPi3,
  DAQ_Globals,
  ADS8320,
  BMP280,
  SampleBufferUnit,
  Console;

function Find_DataDrive:LongWord;
procedure Prepare_DataFolders;
procedure init_Devices;

implementation

function Find_DataDrive:LongWord;
var
 DirsToCheck:array[1..6] of String=('C:\','D:\','E:\','F:\','A:\','B:\');
 DirToCheck:String;
 SearchRec:TSearchRec;
 Count:LongWord;
 N_Drive:LongWord;
begin
N_Drive:=0;
DATA_DRIVE:='';
OS_DRIVE:='';

while not DirectoryExists('C:\') do Sleep(1000);

For Count:=1 to length(DirsToCheck) do
 begin
 DirToCheck:=DirsToCheck[Count];
 if DirectoryExists(DirToCheck) then
  begin
   Inc(N_Drive);
   if FileExists(DirToCheck+'kernel7.img') then
    begin
     OS_DRIVE:=DirToCheck;
     if FindFirst((DirToCheck+'BOX_*'),faAnyFile,SearchRec) = 0 then
      begin
       BOX_N:=SearchRec.Name[5..length(SearchRec.Name)];
      end
     else BOX_N:='0';
    end;
   {if FindFirst((DirToCheck+'*.*'),faAnyFile,SearchRec) = 0 then
    begin
     //If FindFirst succeeds it will return 0 and we can proceed with the search
     repeat
      if (SearchRec.Name='kernel7.img') or (SearchRec.Name='boot') then
       begin
        OS_DRIVE:=DirToCheck;
       end;

     //We keep calling FindNext until there are no more files to find
     until (FindNext(SearchRec) <> 0);
    end;}
//  if not ThisDirIsOS then DATA_DRIVE:= DirToCheck;
//  end;
   FindClose(SearchRec);
   if DirToCheck<>OS_DRIVE then DATA_DRIVE:= DirToCheck;
  end;

 end;
 if DATA_DRIVE='' then
  begin
   DATA_DRIVE:=OS_DRIVE;
   Result:=1;
  end
 else Result:=0;
end;
procedure Prepare_DataFolders;
begin
DATA_DIR:=DATA_DRIVE+'Data_'+BOX_N+'\';

if not DirectoryExists(DATA_DIR) then CreateDir(DATA_DIR);
DATA_DIR:=DATA_DIR+FormatDateTime('yyyy_mm_dd__hh-mm-ss',Now)+'\';
CreateDir(DATA_DIR);
if not DirectoryExists(DATA_DIR+'ADC\') then CreateDir(DATA_DIR+'ADC\');
if not DirectoryExists(DATA_DIR+'BMP\') then CreateDir(DATA_DIR+'BMP\');
end;

procedure init_Devices;
var
 mode:Byte;
 i:LongWord;
begin
{Set GPIO pin 17 which is our switch to Pull Up so that when the switch is open
the value read from the pin will be High}
GPIOPullSelect(GPIO_PIN_17,GPIO_PULL_DOWN);
GPIOFunctionSelect(GPIO_PIN_17,GPIO_FUNCTION_IN);

ADCDevice_glob:=ADC_startDevice;

mode:=1;
// if mode=0 then BMPDevice_glob:=ADCDevice
//BMPDevice_glob:=BMPI2CDevice;
BMP280_startDevice(BMPDevice_glob,mode);

{Display a startup message on the console}
ConsoleWindowWriteLn(LeftWindow,'Preparing data sampling');

SampleBuffer:=BufferArrayInit(SampleBuffer);
BMPSampleBuffer:=BMP_BufferArrayInit(BMPSampleBuffer);

for i:=1 to N_BUFFER do
 ConsoleWindowWriteLn(LeftWindow,'Buffer: '+IntToStr(i)+' Size: '+IntToStr(SizeOf(SampleBuffer[i]^)));

{Start our dedicated CPU thread that does nothing but taking samples and write it to the buffer(s)}
READY_SWITCH:=False;
end;

procedure Process_Config(OneDrive:LongWord);
//var

 begin
  if OneDrive=0 then
   begin

   end;
 end;

end.

