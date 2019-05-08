package pl.edu.agh.io.umniedziala.activeApplicationMonitor;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;

public class ActiveApplicationListener extends Thread {
    private final int MAX_TITLE_LENGTH = 1024;

    private final ApplicationRunningPeriodsManager programRunningPeriodsManager;


    public ActiveApplicationListener() {

        this.programRunningPeriodsManager = new ApplicationRunningPeriodsManager();
    }

    public void run() {
        while (true) {
            String windowName = getCurrentActiveWindowName();
            String appName = windowName.split("\\\\")[windowName.split("\\\\").length - 1];

            // debuging ======
            System.out.println("Active window title: " + windowName);
            System.out.println("App name: " + appName);
            System.out.println("==================");
            // ===============

            this.programRunningPeriodsManager.handleApplicationRunningPeriod(appName);

            try {
                // change interval in which periods want to be updated
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


    private String getCurrentActiveWindowName() {
        char[] buffer = new char[MAX_TITLE_LENGTH * 2];
        WinDef.HWND hwnd = User32.INSTANCE.GetForegroundWindow();
        User32.INSTANCE.GetWindowText(hwnd, buffer, MAX_TITLE_LENGTH);

        String fgImageName = getImageName(hwnd);
        if (fgImageName == null) {
            return "Application not found";
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
}
