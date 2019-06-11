package pl.edu.agh.io.umniedziala.monitors.compuerMonitor;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;
import org.jnativehook.mouse.NativeMouseEvent;
import org.jnativehook.mouse.NativeMouseInputListener;
import pl.edu.agh.io.umniedziala.configuration.Configuration;
import pl.edu.agh.io.umniedziala.model.ComputerRunningPeriodEntity;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ActivityListener extends Thread implements NativeMouseInputListener, NativeKeyListener {

    private long inactivityDuration;

    private static long startTime;

    private static volatile boolean activeAgain = false;

    private volatile boolean exit = false;

    private static DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private int lastRunningPeriodId;
    private String start;

    public ActivityListener() {
        inactivityDuration = Configuration.getInstance().getInactivityPeriod();
    }

    public void nativeMouseClicked(NativeMouseEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeMousePressed(NativeMouseEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);    }

    public void nativeMouseReleased(NativeMouseEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeMouseMoved(NativeMouseEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeMouseDragged(NativeMouseEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeKeyPressed(NativeKeyEvent e) {
        if (e.getKeyCode() == NativeKeyEvent.VC_ESCAPE) {
            try {
                GlobalScreen.unregisterNativeHook();
            } catch (NativeHookException e1) {
                e1.printStackTrace();
            }
        }
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeKeyReleased(NativeKeyEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    public void nativeKeyTyped(NativeKeyEvent e) {
        startTime = System.nanoTime();
        setActiveAgain(true);
    }

    private void setActiveAgain(boolean active) {
        activeAgain = active;
    }

    public void run() {
        try {
            GlobalScreen.registerNativeHook();
        }
        catch (NativeHookException ex) {
            System.err.println("There was a problem registering the native hook.");
            System.err.println(ex.getMessage());
            System.exit(1);
        }

        ActivityListener activityListener = new ActivityListener();
        GlobalScreen.addNativeMouseListener(activityListener);
        GlobalScreen.addNativeMouseMotionListener(activityListener);
        GlobalScreen.addNativeKeyListener(activityListener);

        Logger logger = Logger.getLogger(GlobalScreen.class.getPackage().getName());
        logger.setLevel(Level.OFF);

        Handler[] handlers = Logger.getLogger("").getHandlers();
        for (int i = 0; i < handlers.length; i++) {
            handlers[i].setLevel(Level.OFF);
        }

        start = dateFormat.format(new Date());
        lastRunningPeriodId = ComputerRunningPeriodEntity.create(start, start).get().getId();
        System.out.println("ACTIVE: " + dateFormat.format(new Date()));

        startTime  = System.nanoTime();

        while(!exit){

            long est = System.nanoTime() - startTime;

            if(est / 1e9 > inactivityDuration){
                activityListener.setActiveAgain(false);
                System.out.println("INACTIVE: " + dateFormat.format(new Date()));
                ComputerRunningPeriodEntity.update(lastRunningPeriodId, start, dateFormat.format(new Date()));

                while(true){

                    if(activeAgain){
                        start = dateFormat.format(new Date());
                        lastRunningPeriodId = ComputerRunningPeriodEntity.create(start, start).get().getId();
                        System.out.println("ACTIVE AGAIN: " + dateFormat.format(new Date()));
                        break;
                    }

                    try{
                        Thread.sleep(500);
                    } catch (InterruptedException ex){
                        ex.printStackTrace();
                    }

                }
            }

            try{
                Thread.sleep(1000);
            } catch (InterruptedException ex){
                ex.printStackTrace();
            }

            ComputerRunningPeriodEntity.update(lastRunningPeriodId, start, dateFormat.format(new Date()));

        }

    }

}
