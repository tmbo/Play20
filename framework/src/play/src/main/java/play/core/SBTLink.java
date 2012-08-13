package play.core;

import java.io.*;

/**
 * Generic interface that helps the communication between a Play Application
 * and the underlying SBT infrastructre.
 *
 * Unfortunately it has to be written in Java, so we are not dependent of the Scala version used by SBT.
 */
public interface SBTLink {

	// Will return either:
	// - Throwable -> If something is wrong
	// - ClassLoader -> If the classLoader changed
	// - null -> if nothing changed
	public Object reload();

	// Can return null
	public File findSource(String className);

	public File projectPath();

	public Object runTask(String name);

	public void forceReload();

	public String markdownToHtml(String markdown);

}