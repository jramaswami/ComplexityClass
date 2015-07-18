import org.nlogo.app.App;

public class RunSim {
	public static void main(String[] argv) {
		App.main(argv);
		try {
			java.awt.EventQueue.invokeAndWait(
				new Runnable() {
					public void run() {
						try {
							App.app().open("/Users/jramaswami/Documents/MOOCs/Complexity/Unit1/MultipleAnts.nlogo");
						}
						catch (java.io.IOException ex) {
							ex.printStackTrace();
						}
					}
				}
			);
			App.app().command("set population 50");
			App.app().command("set max-turn-angle 60");
			App.app().command("set max-step-size 4");
			for (int i = 0; i < 5; i++) {
				App.app().command("setup");
				App.app().command("go");
				System.out.println(App.app().report("ticks"));
			}
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
}
