unit UltimateGPS;

{$mode delphi}
{$H+}
{$inline on}   {Allow use of Inline procedures}
interface

uses
  Classes,
  SysUtils,
  Platform,
  GlobalConst,
  GlobalConfig,
  GlobalTypes,
  Console,
  StrUtils,
  DAQ_Globals;

type
  TGPSMessage=array[1..20] of String;
  PGPSMessage= ^TGPSMessage;

  TGPSPosition=array[1..3] of String;
function Get_GPS_Data:TGPSMessage;
function Read_GPSWord(var EOM:Boolean):String;
function Read_GPSMessage(var GPSMessageID:String; var WordCount:LongWord):TGPSMessage;
function Get_GPSMessageWithID(GPSMessageID:String):TGPSMessage;
Function Get_GPSFixStatus:Byte;
procedure WaitFor_GPSFix;
function Get_GPSDateTime(GPSMessagePointer:PGPSMessage):TDateTime;
function Set_TimeToGPSTime(GPSMessagePointer:PGPSMessage):TDateTime;
procedure Save_GPSPosition;
function Init_GPSData:TDateTime;
implementation

function Get_GPS_Data:TGPSMessage;
var
 GPSMessageID:String;
 WordCount:LongWord;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
begin
 MessageFound:=False;
 WordCount:=0;
 GPSMessageID:='';
 SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 while not(MessageFound) do
  begin
    GPSMessage:=Read_GPSMessage(GPSMessageID,WordCount);
    ConsoleWindowWriteLn(LeftWindow,GPSMessageID + '  '+IntToStr(WordCount));
    if (GPSMessageID = '$GPGGA') and (WordCount=15) then MessageFound:=True;
  end;
 SerialClose;
 Result:=GPSMessage;
end;

function Read_GPSWord(var EOM:Boolean):String;
var
 Count:LongWord;
 Character:Char;
 GPSWord:String;
 EOW:Boolean;
begin
 Count:=0;
 EOW:=False;
 GPSWord:='';
 while not(EOW) and not(EOM) do
  begin
    SerialRead(@Character,SizeOf(Character),Count);
    if Character = #13 then EOM:=True
    else if (Character = ',') or (Character = '*') then EOW:=True
    else GPSWord:=GPSWord + Character;
  end;
 Result:=DelChars(GPSWord,#10);
end;
function Read_GPSMessage(var GPSMessageID:String; var WordCount:LongWord):TGPSMessage;
var
 EOM:Boolean;
 GPSMessage:TGPSMessage;
 GPSWord:String;
 i,Count:LongWord;
begin
 EOM:=False;
 GPSMessageID:='';
 Count:=0;
 for i:=1 to length(GPSMessage) do GPSMessage[i]:='';
 while not(EOM) do
  begin
    Inc(Count);
    GPSWord:=Read_GPSWord(EOM);
    //if not EOM then
    //begin
     WordCount:=Count;
     GPSMessage[WordCount]:=GPSWord;
    //end;

  end;
  GPSMessageID:=GPSMessage[1];
  Result:=GPSMessage;
end;
function Get_GPSMessageWithID(GPSMessageID:String):TGPSMessage;
const
 AllGPSMessageIDs    : array[1..5] of String = ('$GPGGA','$GPGSA','$GPGSV','$GPRMC','$GPVTG');
 AllGPSMessageLength : array[1..5] of Byte = (16,19,21,14,11);
var
 WordCount,WordCurrentCount,i:LongWord;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
 GPSMessageCurrentID:String;
begin
 MessageFound:=False;
 WordCurrentCount:=0;
 i:=AnsiIndexText(GPSMessageID,AllGPSMessageIDs)+1;
 if i>0 then WordCount:=AllGPSMessageLength[i];
 SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 while not(MessageFound) do
  begin
    GPSMessage:=Read_GPSMessage(GPSMessageCurrentID,WordCurrentCount);
    if (GPSMessageCurrentID=GPSMessageID) and (WordCurrentCount=WordCount) then MessageFound:=True;
  end;
 SerialClose;
 Result:=GPSMessage;
end;

Function Get_GPSFixStatus:Byte;
var
 GPSMessage:TGPSMessage;
begin
 GPSMessage:=Get_GPSMessageWithID('$GPGGA');
 Result:=Byte(StrToint(GPSMessage[7]));
end;

procedure WaitFor_GPSFix;
begin
 while Get_GPSFixStatus<1 do Sleep(1000);
end;

function Get_GPSTime(GPSMessagePointer:PGPSMessage):TTime;
var
 WordCount:LongWord;
 GPSMessageID, TimeStr:String;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
begin
 if GPSMessagePointer=nil then
 begin
  MessageFound:=False;
  SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
  while not(MessageFound) do
   begin
    GPSMessage:=Read_GPSMessage(GPSMessageID,WordCount);
    if ((GPSMessageID='$GPGGA') and (WordCount=16)) or ((GPSMessageID='$GPRMC') and (WordCount=14)) then MessageFound:=True;
   end;
  end
 else
  begin
   GPSMessage:=GPSMessagePointer^;
  end;
 TimeStr:=GPSMessage[2];
 Result:=EncodeTime(StrToInt(TimeStr[1..2]),StrToInt(TimeStr[3..4]),StrToInt(TimeStr[5..6]),StrToInt(TimeStr[8..10]));
end;

function Get_GPSDateTime(GPSMessagePointer:PGPSMessage):TDateTime;
var
 TimeStr,DateStr:String;
 GPSDateTS,GPSTimeTS:TTimeStamp;
 GPSDate,GPSTime:TDateTime;
 GPSMessage:TGPSMessage;
begin
 if GPSMessagePointer=nil then GPSMessage:=Get_GPSMessageWithID('$GPRMC') else GPSMessage:=GPSMessagePointer^;
 TimeStr:=GPSMessage[2];
 DateStr:=GPSMessage[10];
 GPSTime:=EncodeTime(StrToInt(TimeStr[1..2]),StrToInt(TimeStr[3..4]),StrToInt(TimeStr[5..6]),StrToInt(TimeStr[8..10]));
 GPSDate:=EncodeDate(2000+StrToInt(DateStr[5..6]),StrToInt(DateStr[3..4]),StrToInt(DateStr[1..2]));
 GPSTimeTS:=DateTimeToTimeStamp(GPSTime);
 GPSDateTS:=DateTimeToTimeStamp(GPSDate);
 GPSDateTS.Time:=GPSTimeTS.Time;
 Result:=TimeStampToDateTime(GPSDateTS);
end;

function Set_TimeToGPSTime(GPSMessagePointer:PGPSMessage):TDateTime;
var
 GPSDateTime:TDateTime;
 SetTime:Int64;
begin
 GPSDateTime:=Get_GPSDateTime(GPSMessagePointer);
 ClockSetTime((Trunc(GPSDateTime*TIME_TICKS_PER_DAY)+TIME_TICKS_TO_1899),True);
 Result:=GPSDateTime;
end;

function Get_GPSPosition(GPSMessagePointer:PGPSMessage):TGPSPosition;
var
 GPSPosition:TGPSPosition;
 GPSMessage:TGPSMessage;
begin
 if GPSMessagePointer=nil then GPSMessage:=Get_GPSMessageWithID('$GPGGA') else GPSMessage:=GPSMessagePointer^;
 GPSPosition[1]:='Lat:'+GPSMessage[3]+GPSMessage[4];
 GPSPosition[2]:='Lon:'+GPSMessage[5]+GPSMessage[6];
 GPSPosition[3]:='Alt:'+GPSMessage[10]+'m';
 Result:=GPSPosition;
end;

function Init_GPSData:TDateTime;

begin
  WaitFor_GPSFix;
  Result:=Set_TimeToGPSTime(nil);
end;

procedure Save_GPSPosition;
var
 GPSPosition:TGPSPosition;
 GPSPosStr:String;
 i:LongWord;
 fname:String;
 tf:TextFile;
 append2File:Boolean;
begin
 append2File:=False;
 GPSPosition:=Get_GPSPosition(nil);

 GPSPosStr:=FormatDateTime('dd.mm.yyyy, hh:mm:ss',Now) + ';  GPS Position:';
 for i:=1 to length(GPSPosition) do GPSPosStr:=GPSPosStr+'  '+GPSPosition[i]+';';

  fname:=DATA_DIR+'GPS_Position_'+BOX_N+'.txt';

  if FileExists(fname) then append2File:=True;

  AssignFile(tf,fname);
  if append2File then append(tf)
  else rewrite(tf);

  writeln(tf,GPSPosStr);

  CloseFile(tf);
 end;
end.

