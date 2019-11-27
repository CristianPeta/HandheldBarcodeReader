package com.winarhi.nativeactivitysubclass;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.io.IOException;

import com.honeywell.decodemanager.DecodeManager;
import com.honeywell.decodemanager.barcode.DecodeResult;

import com.honeywell.decodemanager.SymbologyConfigs;
import com.honeywell.decodemanager.barcode.CommonDefine;
import com.honeywell.decodemanager.symbologyconfig.SymbologyConfigCodeDataMatrix;

//for 75e
import com.honeywell.aidc.*;

//Zebra
import com.symbol.emdk.EMDKManager;
import com.symbol.emdk.EMDKResults;
import com.symbol.emdk.EMDKManager.EMDKListener;
import com.symbol.emdk.EMDKManager.FEATURE_TYPE;
import com.symbol.emdk.barcode.BarcodeManager;
import com.symbol.emdk.barcode.BarcodeManager.ConnectionState;
import com.symbol.emdk.barcode.BarcodeManager.ScannerConnectionListener;
import com.symbol.emdk.barcode.ScanDataCollection;
import com.symbol.emdk.barcode.Scanner;
import com.symbol.emdk.barcode.ScannerConfig;
import com.symbol.emdk.barcode.ScannerException;
import com.symbol.emdk.barcode.ScannerInfo;
import com.symbol.emdk.barcode.ScannerResults;
import com.symbol.emdk.barcode.ScanDataCollection.ScanData;
import com.symbol.emdk.barcode.Scanner.DataListener;
import com.symbol.emdk.barcode.Scanner.StatusListener;
import com.symbol.emdk.barcode.Scanner.TriggerType;
import com.symbol.emdk.barcode.StatusData.ScannerStates;
import com.symbol.emdk.barcode.StatusData;

import android.os.Build;
import android.os.Handler;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;
import android.os.Bundle;
import android.content.Intent;
import android.widget.Toast;

public class NativeActivitySubclass extends com.embarcadero.firemonkey.FMXNativeActivity implements
    BarcodeReader.BarcodeListener, BarcodeReader.TriggerListener,
    EMDKListener, DataListener, StatusListener, ScannerConnectionListener //Zebra
{

    //private final int ID_SCANSETTING = 0x12;
    //private final int ID_CLEAR_SCREEN = 0x13;

    static final String TAG = "NativeActivitySubclass";
    private DecodeManager mDecodeManager = null;
    public String strDecodeResult = "";
    private final int WA_SCANTIMEOUT = 5000;
    long WA_mScanAccount = 0;
    private NativeActivitySubclass selfActivity;

    //for 75e
    private com.honeywell.aidc.BarcodeReader H75e_barcodeReader = null;
    private AidcManager H75e_manager = null;

    //Zebra
    private EMDKManager emdkManager = null;
    private BarcodeManager barcodeManager = null;
    private Scanner scanner = null;

    private boolean bContinuousMode = false;

    private List<ScannerInfo> deviceList = null;

    private int scannerIndex = 0; // Keep the selected scanner
    private int defaultIndex = 0; // Keep the default scanner
    private int triggerIndex = 0;
    private int dataLength = 0;
    private String statusString = "";

    private String [] triggerStrings = {"HARD", "SOFT"};
    //END Zebra

    public native void onBarCodeCompleteNative(String BarCode);
    //public native void onBarCodeFailNative();
    public native void onScannerStatusNative(String Status);

    //introduced for XE7
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //Custom initialization
        Log.d(TAG, "onCreate");

        //Zebra
	deviceList = new ArrayList<ScannerInfo>();

        selfActivity = this;

        //Se creaza la TBarCodeScanner.Create()
        //WA_Zebra_Create_EMDKManager();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        WA_Zebra_Destroy_EMDKManager();
    }

    //for 70e
    private void WA_70e_CreateDecodeManager() {
        if (mDecodeManager == null)
            mDecodeManager = new DecodeManager(this, ScanResultHandler);
    }

    //for 70e
    private void WA_70e_DestroyDecodeManager() {
        if (mDecodeManager != null) {
            try {
                mDecodeManager.release();
                mDecodeManager = null;
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    //for 70e
    private void WA_70e_DoTriggerScan() throws Exception {
        if (mDecodeManager != null) {
            try {
                strDecodeResult = "";
                mDecodeManager.doDecode(WA_SCANTIMEOUT);
            } catch (RemoteException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    //for 70e
    private Handler ScanResultHandler = new Handler() {
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case DecodeManager.MESSAGE_DECODER_COMPLETE:
                    WA_mScanAccount++;
                    DecodeResult decodeResult = (DecodeResult) msg.obj;

                    //byte codeid = decodeResult.codeId;
                    //byte aimid = decodeResult.aimId;
                    //int iLength = decodeResult.length;

                    strDecodeResult = decodeResult.barcodeData;
                    onBarCodeCompleteNative(strDecodeResult);
                    break;

                case DecodeManager.MESSAGE_DECODER_FAIL: {
                    strDecodeResult = "";
                    //onBarCodeFailNative();
                }
                break;
                case DecodeManager.MESSAGE_DECODER_READY:
                {
                    try {
                        //mDecodeManager.disableSymbology(CommonDefine.SymbologyID.SYM_ALL);
                        SymbologyConfigCodeDataMatrix DataMatrix = new SymbologyConfigCodeDataMatrix();
                        DataMatrix.enableSymbology(true);
                        SymbologyConfigs symconfig = new SymbologyConfigs();

                        symconfig.addSymbologyConfig(DataMatrix);
                        mDecodeManager.setSymbologyConfigs(symconfig);

                        mDecodeManager.enableSymbology(CommonDefine.SymbologyID.SYM_DATAMATRIX); //sau SYM_ALL
                    } catch (RemoteException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
                break;
                default:
                    super.handleMessage(msg);
                    break;
            }
        }
    };

    //for 70e
    private void WA_cancelScan() throws Exception {
        mDecodeManager.cancelDecode();
    }

    //for 75e
    private void WA_75e_Create_aidcManager() {
        // create the AidcManager providing a Context and a
        // CreatedCallback implementation.
        if (H75e_manager == null)
          AidcManager.create(this, new AidcManager.CreatedCallback() {
              @Override
              public void onCreated(AidcManager aidcManager) {
                  H75e_manager = aidcManager;
                  H75e_barcodeReader = H75e_manager.createBarcodeReader();

                  // register bar code event listener
                  H75e_barcodeReader.addBarcodeListener(selfActivity);

                  // set the trigger mode to client control
                  try {
                        H75e_barcodeReader.setProperty(BarcodeReader.PROPERTY_TRIGGER_CONTROL_MODE,
                                BarcodeReader.TRIGGER_CONTROL_MODE_AUTO_CONTROL);
                        H75e_barcodeReader.setProperty(BarcodeReader.PROPERTY_QR_CODE_ENABLED, true);
                  } catch (UnsupportedPropertyException e) {
                        Toast.makeText(selfActivity, "Failed to apply properties", Toast.LENGTH_SHORT).show();
                  }
                  // register trigger state change listener
                  H75e_barcodeReader.addTriggerListener(selfActivity);

                  try {
                      H75e_barcodeReader.claim();
                  } catch (ScannerUnavailableException e) {
                      e.printStackTrace();
                      Toast.makeText(selfActivity, "Scanner unavailable", Toast.LENGTH_SHORT).show();
                  }
              }
          });
    }

    //for 75e
    private void WA_75e_Destroy_aidcManager() {
        if (H75e_barcodeReader != null) {
            // unregister barcode event listener
            H75e_barcodeReader.removeBarcodeListener(this);

            // unregister trigger state change listener
            H75e_barcodeReader.removeTriggerListener(this);

            // close BarcodeReader to clean up resources.
            H75e_barcodeReader.close();
            H75e_barcodeReader = null;
        }

        if (H75e_manager != null) {
            // close AidcManager to disconnect from the scanner service.
            // once closed, the object can no longer be used.
            H75e_manager.close();
            H75e_manager = null;
        }
    }

    //for 75e BarcodeReader.BarcodeListener.onBarcodeEvent
    @Override
    public void onBarcodeEvent(final BarcodeReadEvent event) {
        onBarCodeCompleteNative(event.getBarcodeData());
    }

    //for 75e BarcodeReader.BarcodeListener.onFailureEvent
    @Override
    public void onFailureEvent(BarcodeFailureEvent arg0) {
        // TODO Auto-generated method stub
        Toast.makeText(this, "BarcodeListener.onFailureEvent", Toast.LENGTH_SHORT).show();

    }

    @Override
    public void onPause() {
        super.onPause();

       Log.d(TAG, "onPause");

       //for 75e
        if (H75e_barcodeReader != null) {
            // release the scanner claim so we don't get any scanner
            // notifications while paused.
            H75e_barcodeReader.release();
        }

        //for Zebra
        deInitScanner();
        if (barcodeManager != null) {
            barcodeManager.removeConnectionListener(this);
            barcodeManager = null;
            deviceList = null;
        }
        // Release the barcode manager resources
        if (emdkManager != null) {
            emdkManager.release(FEATURE_TYPE.BARCODE);
        }
    }

    @Override
    public void onResume() {
        super.onResume();

        //for 75e
        if (H75e_barcodeReader != null) {
            try {
                H75e_barcodeReader.claim();
            } catch (ScannerUnavailableException e) {
                e.printStackTrace();
                Toast.makeText(this, "Scanner unavailable", Toast.LENGTH_SHORT).show();
            }
        }

        //for Zebra - Acquire the barcode manager resources
        if (emdkManager != null) {
            barcodeManager = (BarcodeManager) emdkManager.getInstance(FEATURE_TYPE.BARCODE);

            // Add connection listener
            if (barcodeManager != null) {
                barcodeManager.addConnectionListener(this);
            }

            // Enumerate scanner devices
            enumerateScannerDevices();

            // Initialize scanner
            initScanner();
            setTrigger();
            Zebra_setDecoders();

            //Start scan
            Zebra_startScan();//start scan automaticaly (scan will actually start when the user press the button)
        }
    }

    //for 75e BarcodeReader.TriggerListener.onTriggerEvent
    @Override
    public void onTriggerEvent(TriggerStateChangeEvent event) {
        Toast.makeText(this, "TriggerListener.onTriggerEvent", Toast.LENGTH_SHORT).show();

    }

//-------------- ZEBRA

    //Zebra
    private void WA_Zebra_Create_EMDKManager() {
        onScannerStatusNative("S1: WA_Zebra_Create_EMDKManager");
        EMDKResults results = EMDKManager.getEMDKManager(getApplicationContext(), selfActivity);
        if (results.statusCode != EMDKResults.STATUS_CODE.SUCCESS) {
                onScannerStatusNative("1: EMDKManager object request failed!");
                return;
        }
    }

    //Zebra
    private void WA_Zebra_Destroy_EMDKManager() {
        onScannerStatusNative("S2: WA_Zebra_Destroy_EMDKManager");

        //ZEBRA
        // De-initialize scanner
        deInitScanner();

        // Remove connection listener
        if (barcodeManager != null) {
            barcodeManager.removeConnectionListener(this);
            barcodeManager = null;
        }

        // Release all the resources
        if (emdkManager != null) {
            emdkManager.release();
            emdkManager = null;
        }
    }

    //Zebra EMDKListener.onOpened
    @Override
    public void onOpened(EMDKManager emdkManager) {

        onScannerStatusNative("2: EMDK open success!");

        this.emdkManager = emdkManager;

        // Acquire the barcode manager resources
        barcodeManager = (BarcodeManager) emdkManager.getInstance(FEATURE_TYPE.BARCODE);

        // Add connection listener
        if (barcodeManager != null) {
            barcodeManager.addConnectionListener(this);
        }

        // Enumerate scanner devices
        enumerateScannerDevices();

        // Set default scanner
        //spinnerScannerDevices.setSelection(defaultIndex);

       if (scanner == null) {
           initScanner();
           setTrigger();
           Zebra_setDecoders();
           Zebra_startScan();//start scan automaticaly (scan will actually start when the user press the button)
       }
    }

    //Zebra EMDKListener.onClosed
    @Override
    public void onClosed() {

        if (emdkManager != null) {

            // Remove connection listener
            if (barcodeManager != null){
                barcodeManager.removeConnectionListener(this);
                barcodeManager = null;
            }

            // Release all the resources
            emdkManager.release();
            emdkManager = null;
        }
        onScannerStatusNative("3: EMDK closed unexpectedly! Please close and restart the application.");
    }

    //Zebra DataListener.onData
    @Override
    public void onData(ScanDataCollection scanDataCollection) {

        if ((scanDataCollection != null) && (scanDataCollection.getResult() == ScannerResults.SUCCESS)) {
            ArrayList <ScanData> scanData = scanDataCollection.getScanData();
            for(ScanData data : scanData) {
                 onBarCodeCompleteNative(data.getData());
            }
        }
    }

    //Zebra StatusListener.onStatus
    @Override
    public void onStatus(StatusData statusData) {

        ScannerStates state = statusData.getState();
        switch(state) {
            case IDLE:
                statusString = statusData.getFriendlyName()+" is enabled and idle...";
                onScannerStatusNative("4: " + statusString);
                if (bContinuousMode) {
                    try {
                        // An attempt to use the scanner continuously and rapidly (with a delay < 100 ms between scans)
                        // may cause the scanner to pause momentarily before resuming the scanning.
                        // Hence add some delay (>= 100ms) before submitting the next read.
                        try {
                            Thread.sleep(100);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        scanner.read();
                    } catch (ScannerException e) {
                        statusString = e.getMessage();
                        onScannerStatusNative("5: " + statusString);
                    }
                }
                break;
            case WAITING:
                statusString = "Scanner is waiting for trigger press...";
                onScannerStatusNative("6: " + statusString);
                break;
            case SCANNING:
                statusString = "Scanning...";
                onScannerStatusNative("7: " + statusString);
                break;
            case DISABLED:
                statusString = statusData.getFriendlyName()+" is disabled.";
                onScannerStatusNative("8: " + statusString);
                break;
            case ERROR:
                statusString = "An error has occurred.";
                onScannerStatusNative("9: " + statusString);
                break;
            default:
                break;
        }
    }

    //Zebra ScannerConnectionListener.onConnectionChange
    @Override
    public void onConnectionChange(ScannerInfo scannerInfo, ConnectionState connectionState) {

        String scannerName = "";

        String statusExtScanner = connectionState.toString();
        String scannerNameExtScanner = scannerInfo.getFriendlyName();

        if (deviceList.size() != 0) {
            scannerName = deviceList.get(scannerIndex).getFriendlyName();
        }

        if (scannerName.equalsIgnoreCase(scannerNameExtScanner)) {

            switch(connectionState) {
                case CONNECTED:
                    deInitScanner();
                    initScanner();
                    setTrigger();
                    Zebra_setDecoders();
                    break;
                case DISCONNECTED:
                    deInitScanner();
                    break;
            }

            onScannerStatusNative("10: " + scannerNameExtScanner + ":" + statusExtScanner);
        }
        else {
            onScannerStatusNative("11: " + statusString + " " + scannerNameExtScanner + ":" + statusExtScanner);
        }
    }

    //Zebra (local function)
    private void enumerateScannerDevices() {

        if (barcodeManager != null) {

            List<String> friendlyNameList = new ArrayList<String>();
            int spinnerIndex = 0;

            deviceList = barcodeManager.getSupportedDevicesInfo();

            if ((deviceList != null) && (deviceList.size() != 0)) {

                Iterator<ScannerInfo> it = deviceList.iterator();
                while(it.hasNext()) {
                    ScannerInfo scnInfo = it.next();
                    friendlyNameList.add(scnInfo.getFriendlyName());
                    if(scnInfo.isDefaultScanner()) {
                        defaultIndex = spinnerIndex;
                    }
                    ++spinnerIndex;
                }
            }
            else {
                onScannerStatusNative("12: " + "Failed to get the list of supported scanner devices! Please close and restart the application.");
            }
        }
    }

    //Zebra (local function)
    private void setTrigger() {

        if (scanner == null) {
            initScanner();
        }

        if (scanner != null) {
            switch (triggerIndex) {
                case 0: // Selected "HARD"
                    scanner.triggerType = TriggerType.HARD;
                    break;
                case 1: // Selected "SOFT"
                    scanner.triggerType = TriggerType.SOFT_ALWAYS;
                    break;
            }
        }
    }

    //Zebra (local function)
    private void Zebra_setDecoders() {
        //if (1==1) {return;}
        if (scanner == null) {
            initScanner();
        }

        if ((scanner != null) && (scanner.isEnabled())) {
            try {

                ScannerConfig config = scanner.getConfig();

                // Set EAN8
                config.decoderParams.ean8.enabled = false;

                // Set EAN13
                config.decoderParams.ean13.enabled = false;

                // Set Code39
                config.decoderParams.code39.enabled = true;

                //Set Code128
                config.decoderParams.code128.enabled = true;

                //Set Interleaved 2of5
                config.decoderParams.i2of5.enabled = true;

                //config.decoderParams.i2of5.checkdigit = 0;
                //config.decoderParams.i2of5.length1 = 6;
                //config.decoderParams.i2of5.length2 = 55;
                //config.decoderParams.i2of5.redundancy = true;
                //config.decoderParams.i2of5.report_check_digit = false;

                //Set Discrete 2of5
                config.decoderParams.d2of5.enabled = true;

                //config.decoderParams.chinese2of5.enabled = true;
                //config.decoderParams.matrix2of5.enabled = true;

                scanner.setConfig(config);

            } catch (ScannerException e) {

                onScannerStatusNative("13: " + e.getMessage());
            }
        }
    }


    //Zebra (local function)
    private void Zebra_startScan() {

        if(scanner == null) {
            initScanner();
        }

        if (scanner != null) {
            try {

				if(scanner.isEnabled())
				{
					// Submit a new read.
					scanner.read();

					//if (checkBoxContinuous.isChecked())
						bContinuousMode = true;
					//else
					//	bContinuousMode = false;
				}
				else
				{
					onScannerStatusNative("14: " + "Scanner is not enabled");
				}

            } catch (ScannerException e) {

                onScannerStatusNative("15: " + e.getMessage());
            }
        }

    }

    //Zebra (local function)
    private void Zebra_stopScan() {

        if (scanner != null) {

            try {

                // Reset continuous flag
                bContinuousMode = false;

                // Cancel the pending read.
                scanner.cancelRead();

            } catch (ScannerException e) {

                onScannerStatusNative("16: " + e.getMessage());
            }
        }
    }

    //Zebra (local function)
    private void initScanner() {
        onScannerStatusNative("S3: initScanner()");

        if (scanner == null) {

            if ((deviceList != null) && (deviceList.size() != 0)) {
                scanner = barcodeManager.getDevice(deviceList.get(scannerIndex));
                onScannerStatusNative("S4: initScanner() scanner received");
            }
            else {
                onScannerStatusNative("17: " + "Failed to get the specified scanner device! Please close and restart the application.");
                return;
            }

            if (scanner != null) {

                scanner.addDataListener(this);
                scanner.addStatusListener(this);

                try {
                    onScannerStatusNative("S5: initScanner() enabling scanner ");
                    scanner.enable();
                    onScannerStatusNative("S6: initScanner() scanner enabled");
                } catch (ScannerException e) {

                    onScannerStatusNative("18: " + e.getMessage());
                }
            }else{
                onScannerStatusNative("19: " + "Failed to initialize the scanner device.");
            }
        }
    }

    //Zebra (local function)
    private void deInitScanner() {

        if (scanner != null) {

            try {

                scanner.cancelRead();
                scanner.disable();

			} catch (Exception e) {

                onScannerStatusNative("20: " + e.getMessage());
            }

			try {
				scanner.removeDataListener(this);
                scanner.removeStatusListener(this);

            } catch (Exception e) {

                onScannerStatusNative("21: " + e.getMessage());
            }

            try{
                scanner.release();
            } catch (Exception e) {

                onScannerStatusNative("22: " + e.getMessage());
            }

            scanner = null;
        }
    }

}
