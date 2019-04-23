package pl.edu.agh.io.umniedziala.activeApplicationMonitor;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;

public class ActiveApplicationListener extends Thread {
    private final int MAX_TITLE_LENGTH = 1024;

    private final ApplicationRunningPeriodsManager programRunningPeriodsManager;


    public ActiveApplicationListener() {

        this.programRunningPeriodsManager = new ApplicationRunningPeriodsManager();
    }

    public void run() {
        while (true) {
            String windowName = getCurrentActiveWindowName();
            String appName = windowName.split("-")[windowName.split("-").length - 1].replaceFirst(" ", "");

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

        return Native.toString(buffer);
    }
}
