package pl.edu.agh.io.umniedziala;

import pl.edu.agh.io.umniedziala.activeApplicationMonitor.ActiveApplicationListener;

public class Main {
    public static void main(String[] args) throws InterruptedException {
        new ActiveApplicationListener(5000).start();
    }
}
