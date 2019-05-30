package pl.edu.agh.io.umniedziala.monitors.backgroundApplicationsMonitor;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;
import pl.edu.agh.io.umniedziala.model.ApplicationEntity;
import pl.edu.agh.io.umniedziala.model.BackgroundPeriodEntity;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;
import pl.edu.agh.io.umniedziala.monitors.activeApplicationMonitor.ActiveWindowNotFound;

import java.io.OptionalDataException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

public class BackgroundApplicationsMonitor extends Thread {

    private volatile boolean exit = false;

    private int checkingIntervalInMs;

    private List<ApplicationEntity> applicationEntityList;

                    //app_id : back_id
    private HashMap<Integer, Integer> entitiesMap = new HashMap<>(); //temporary

    private static DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");


    public BackgroundApplicationsMonitor(int checkingIntervalInMs){
        this.applicationEntityList = new ArrayList<>();
        this.checkingIntervalInMs = checkingIntervalInMs;
    }

    private String getCurrentActiveWindowName() throws ActiveWindowNotFound {
        WinDef.HWND hwnd = User32.INSTANCE.GetForegroundWindow();

        String fgImageName = getImageName(hwnd);
        if (fgImageName == null) {
            throw new ActiveWindowNotFound();
        } else {
            return fgImageName;
        }
    }

    private String getImageName(WinDef.HWND hwnd) {
        IntByReference processId = new IntByReference();
        User32.INSTANCE.GetWindowThreadProcessId(hwnd, processId);
        // Open the process to get permissions to the image name
        WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(
                Kernel32.PROCESS_QUERY_LIMITED_INFORMATION,
                false,
                processId.getValue()
        );

        char[] buffer = new char[4096];
        IntByReference bufferSize = new IntByReference(buffer.length);

        boolean success = Kernel32.INSTANCE.QueryFullProcessImageName(processHandle, 0, buffer, bufferSize);

        Kernel32.INSTANCE.CloseHandle(processHandle);

        return success ? new String(buffer, 0, bufferSize.getValue()) : null;
    }

    private void initValues(){

        applicationEntityList = ApplicationEntity.getAllApplications();

        for(ApplicationEntity applicationEntity : applicationEntityList){
            String start = dateFormat.format(new Date());
            int id = BackgroundPeriodEntity.create(start,start,applicationEntity.getId()).get().getId();
            entitiesMap.put(applicationEntity.getId(), id);
        }
    }

    public void run(){

        initValues();

        while(!exit){

            applicationEntityList = ApplicationEntity.getAllApplications();

            for(ApplicationEntity applicationEntity : applicationEntityList){
                int id = applicationEntity.getId();

                if(RunningPeriodEntity.findById(entitiesMap.get(id)).isPresent()){
                    int appId = RunningPeriodEntity.findById(entitiesMap.get(id)).get().getApplicationId();
                    applicationEntityList.remove(ApplicationEntity.findById(appId));
                }
            }

            try {
                applicationEntityList.remove(ApplicationEntity.findByName(getCurrentActiveWindowName()));
            } catch (ActiveWindowNotFound activeWindowNotFound) {
                activeWindowNotFound.printStackTrace();
            }

            for(ApplicationEntity applicationEntity : applicationEntityList){
                int id = applicationEntity.getId();

                if(BackgroundPeriodEntity.findById(entitiesMap.get(id)).isPresent()){

                    BackgroundPeriodEntity.update(entitiesMap.get(id), BackgroundPeriodEntity.findById(entitiesMap.get(id)).get().getStartTime(), dateFormat.format(new Date()));

                } else {
                    String start = dateFormat.format(new Date());
                    BackgroundPeriodEntity.create(start,start,id);
                }
            }

            try{
                Thread.sleep(checkingIntervalInMs);
            } catch (InterruptedException ex){
                ex.printStackTrace();
            }

        }

    }

}
