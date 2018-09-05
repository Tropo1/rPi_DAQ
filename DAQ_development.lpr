program DAQ_development;

//{$mode objfpc}{$H+}
{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
uses
  //BCM2709,      {RPi2}
  //BCM2836,      {RPi2}
  //BCM2710,      {RPi3}
  //BCM2837,      {RPi3}
  InitUnit,     {Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi3, {Include RaspberryPi3 to make sure all standard functions are included}
  GlobalConst,
  GlobalConfig,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  ThreadUnit,         {Include our thread unit which contains most of the example}
  //MeasureUnit,
  SampleBufferUnit,
  //DataFileHandlingUnit,
  Platform,
  DWCOTG,
  FATFS,
  NTFS,
  FileSystem,  {Include the file system core and interfaces}
  DAQ_Globals,
  // Time
  Ultibo,   {The Ultibo unit provides some APIs for getting and setting timezones}
  DateUtils,
  SPI,
  I2C,
  //Serial,
  ADS8320,
  //Services, {The services unit includes the NTP client and will automatically include the network}
  BMP280,
  UltimateGPS,
  DAQ_Setup;

{var
 //LeftWindow:TWindowHandle;
 Counter,i:Integer;
 SampleBytes:PSPIBytes;
 ADCDevice:PSPIDevice;
 n_Bytes:LongWord;
 Sample,Sample2,Sample2a:Word;
 return:Byte;
 p,T:Double;
 raw_t,raw_p : LongWord;
 BMPI2CDevice: PI2CDevice;
 BMPDevice:Pointer;
 mode:Byte;
 LastValue:LongWord;

 Count,WordCount:LongWord;
 Character:Char;
 GPSSentenceType:String;
 GPSData:TGPSMessage;
 GPSDate,GPSTime:TDateTime;
 TimeSet:Int64; }
begin
 {Create a console window to show what is happening}
 LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 Sleep(5000);
 Find_DataDrive;
  ConsoleWindowWriteLn(LeftWindow,OS_DRIVE+'  '+DATA_DRIVE+'  '+BOX_N);
 // --> 1
  // --> 1
// SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 Init_GPSData;
 ConsoleWindowWriteLn(LeftWindow,FormatDateTime('yyyy_mm_dd__hh-mm-ss',Now));
 Prepare_DataFolders;
 Save_GPSPosition;
 // --> 1
 init_Devices;


 //ADCDevice_glob:=ADC_startDevice(SampleBytes_glob);
 {ADCDevice:=ADCDevice_glob;
 SampleBytes:=SampleBytes_glob;
 // This works:
 n_Bytes:=0;
 if SPIDeviceRead(ADCDevice ,SPI_CS_0, SampleBytes, 3, SPI_TRANSFER_NONE,n_Bytes) <> ERROR_SUCCESS then
    ConsoleWindowWriteLn(LeftWindow,'Failed to read ADC');
 Sample:=(((SampleBytes^[0] and 3) << 14) or (SampleBytes^[1] << 6) or (SampleBytes^[2] and (63)));
 ConsoleWindowWriteLn(LeftWindow,'Sample :'+IntToStr(Sample));}
 //mode:=1;
 //n_Bytes:=0;
// if mode=0 then BMPDevice_glob:=ADCDevice
 //BMPDevice_glob:=BMPI2CDevice;
 //BMP280_startDevice(BMPDevice_glob,mode);
 //BMPDevice:=BMPDevice_glob;
 //ConsoleWindowWriteLn(LeftWindow,IntToStr(n_Bytes)+';  '+IntToStr(BMPReadData[1])+'  '+IntToStr(BMPReadData[2])+'  '+IntToStr(BMPReadData[3])+'  '+IntToStr(BMPReadData[4])+'  '+IntToStr(BMPReadData[5])+'  '+  IntToStr(BMPReadData[6])) ;
 //ConsoleWindowWriteLn(LeftWindow,IntToStr(n_Bytes)+';  '+IntToStr(mode)+';  '+IntToStr(BMPCalibData[1])+'  '+IntToStr(BMPCalibData[2])+'  '+IntToStr(BMPCalibData[3])+'  '+IntToStr(BMPCalibData[4])+'  '+IntToStr(BMPCalibData[5])  ) ;

 {for i:=0 to 5 do
 begin
 BMP280_read_data(BMPDevice,p,T,raw_t,raw_p);
 ConsoleWindowWriteLn(LeftWindow,IntToStr(BMPWriteData[0])+'  '+IntToStr(BMPWriteData[1]));
 ConsoleWindowWriteLn(LeftWindow,IntToStr(n_Bytes)+';  '+IntToStr(BMPReadData[1])+'  '+IntToStr(BMPReadData[2])+'  '+IntToStr(BMPReadData[3])+'  '+IntToStr(BMPReadData[4])+'  '+IntToStr(BMPReadData[5])+'  '+  IntToStr(BMPReadData[6])) ;
 ConsoleWindowWriteLn(LeftWindow,intToStr(raw_t)+'   '+intToStr(raw_p)+'   p = '+FloatToStr(p)+' Pa ; T = '+FloatToStr(T)+' C');
 Sleep(500);
 end;}

  //BMP280_write_register(BMPDevice,BMPWriteData);
 //reset SampleBytes
 //SampleBytes^[0]:=0;
 //SampleBytes^[1]:=0;
 //SampleBytes^[2]:=0;
  // This doesn't:
 //Sleep(10);
 //Sample2:=ADC_getSample(ADCDevice ,SampleBytes);
 //Sample2a:=(((SampleBytes^[0] and 3) << 14) or (SampleBytes^[1] << 6) or (SampleBytes^[2] and (63)));



 StartSampleThreads(LeftWindow);

 ThreadHalt(0);

end.
 
