unit DAQ_Globals;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GlobalConfig,
  GlobalTypes,
  ADS8320,
  //BMP280,
  SPI,
  I2C;


//---------------------------- Sample Buffer -----------------------------------
const
 SAMPLE_COUNT = 500000;       // number of samples one buffer can hold
 N_BUFFER     = 10;          // number of buffers to cycle through

 BMP_SAMPLE_COUNT = 1000;
 BMP_N_BUFFER     = 5;
type
 TSample = Word;
 TPPSSample = Byte;
  // Arrays for the Buffer
 TSampleArray = array[0..SAMPLE_COUNT  - 1] of TSample;
 TPPSArray    = array[0..SAMPLE_COUNT - 1] of TPPSSample;


 //The buffer of time values
 TSampleBuffer = record
  Count:LongInt;        // Total number of samples currently in the buffer
  State:Byte;           // State of Buffer: (1):Empty, (2):Reading, (3):Writing, (4): Full
  StartTime:TDateTime;  // A field to store the time when the buffer started to fill
  Samples:TSampleArray; // The array where the samples are stored
  PPS:TPPSArray;        // The array where the PPS samples are stored

  maxDT:Word;           // The largest and smallest time differences between taking samples
  minDT:Word;
  end;

  PSampleBuffer = ^TSampleBuffer ;
  PSampleBufferArray = array[1..N_BUFFER] of PSampleBuffer; // Array of multiple SampleBuffers


 type
  TBMPSample      = array[0..1] of LongWord;
  TBMPSampleArray = array[0..BMP_SAMPLE_COUNT - 1] of TBMPSample;


  TBMPPPSArray    = array[0..BMP_SAMPLE_COUNT - 1] of TPPSSample;

  //The buffer of time values
  TBMPSampleBuffer = record
  Count:LongInt;        // Total number of samples currently in the buffer
  State:Byte;           // State of Buffer: (1):Empty, (2):Reading, (3):Writing, (4): Full
  StartTime:TDateTime;  // A field to store the time when the buffer started to fill
  Samples:TBMPSampleArray; // The array where the samples are stored
  PPS:TBMPPPSArray;

  maxDT:LongWord; // The array where the time differences between taking samples are stored
  minDT:LongWord;
  end;

  PBMPSampleBuffer = ^TBMPSampleBuffer;
  PBMPSampleBufferArray = array[1..BMP_N_BUFFER] of PBMPSampleBuffer;

 type
  TBMP_CalibBytes = array[0..24] of Byte;
  PBMP280_CalibData =  ^TBMP_CalibBytes;

  TBPM_ReadBytes = array[0..6] of Byte;
  PBMP280_ReadData =  ^TBPM_ReadBytes;

  TBPM_WriteBytes = array[0..1] of Byte;
  PBMP280_WriteData =  ^TBPM_WriteBytes;

  TBMP_RegByte = array[0..0] of Byte;
  PBMP280_RegByte = ^TBMP_RegByte;

  TBMP_ReceiveBytes = array[0..23] of Byte;
  PBMP280_ReceiveBytes = ^TBMP_ReceiveBytes;
  // now from ADS8320:
  //TSPIBytes = array[0..2] of Byte;
  //PSPIBytes =  ^TSPIBytes;

//------------------------------------------------------------------------------
const
 NEW_FILE_RATE = 1;        // time between creation of new data file in minutes!



 MS_PER_DAY    = 24*3600*1000;          // miliseconds in a day
 US_PER_DAY    = 24*3600*1000000;       // microseconds in a day
var
 DATA_DIR, DATA_DRIVE, OS_DRIVE:String;

 BMPThread,WriteThread:TThreadHandle;

 READY_SWITCH : Boolean;
 STOP_SWITCH  : Boolean;
 BUFFER_OverFlow, BMP_BUFFER_OverFlow: Boolean;

 CLOCK_OFFSET : Int64;

 SampleBuffer       : PSampleBufferArray;
 BMPSampleBuffer    : PBMPSampleBufferArray;
 SampleBytes_glob   : PSPIBytes;
 ADCDevice_glob     : PSPIDevice;
 BMPDevice_glob     : PI2CDevice;

 BMPCalibData : PBMP280_CalibData;
 BMPReadData  : PBMP280_ReadData;
 BMPWriteData : PBMP280_WriteData;

 BMP_CALIB_T1 : Word;
 BMP_CALIB_T2 : Int16;
 BMP_CALIB_T3 : Int16;

 BMP_CALIB_P1 : Word;
 BMP_CALIB_P2 : Int16;
 BMP_CALIB_P3 : Int16;
 BMP_CALIB_P4 : Int16;
 BMP_CALIB_P5 : Int16;
 BMP_CALIB_P6 : Int16;
 BMP_CALIB_P7 : Int16;
 BMP_CALIB_P8 : Int16;
 BMP_CALIB_P9 : Int16;

 BMPReg       : PBMP280_RegByte;
 BMPRecData   : PBMP280_ReceiveBytes;

 BMP_MODE     : Byte;
 BMP_MEAS_TIME: Real;


 PPS_count:QWord;
 PPS_time:QWord;

 LeftWindow, RightWindow:TWindowHandle;
 Offset,MaxOffset,MinOffset:Integer;
 //CurrentCount:QWord;

 BOX_N:String;
implementation

end.

