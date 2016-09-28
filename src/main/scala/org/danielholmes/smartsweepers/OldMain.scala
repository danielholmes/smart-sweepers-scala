package org.danielholmes.smartsweepers

import javax.swing._
import java.awt._
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.net.URISyntaxException
import java.nio.file.Paths

object OldMain {
  private val szApplicationName: String = "Smart Sweepers v1.0"
  private val szWindowClassName: String = "sweeper"

  private var g_pController: Controller = _

  def main(args: Array[String]) {
    loadInConfigParameters()
    g_pController = new Controller
    val mainPanel: JPanel = new JPanel() {
      override def paint(g: Graphics) {
        super.paint(g)
        g_pController.render(g.asInstanceOf[Graphics2D])
      }
    }
    mainPanel.setSize(CParams.WindowWidth, CParams.WindowHeight)
    val frame: JFrame = new JFrame(szApplicationName)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(CParams.WindowWidth, CParams.WindowHeight)
    frame.getContentPane.add(mainPanel)
    frame.addKeyListener(new KeyListener() {
      def keyReleased(e: KeyEvent) {
      }

      def keyTyped(e: KeyEvent) {
      }

      def keyPressed(e: KeyEvent) {
        if (e.getKeyChar == 'f') g_pController.fastRenderToggle()
        else if (e.getKeyChar == 'r') g_pController = new Controller
        else if (e.getKeyCode == KeyEvent.VK_ESCAPE) System.exit(0)
      }
    })
    frame.setVisible(true)
    val millisPerFrame: Long = 1000 / CParams.iFramesPerSecond
    var done = false
    while (!done) {
      val frameStart: Long = System.currentTimeMillis
      if (!g_pController.update) {
        done = true
        // break //todo: break is not supported
      } else {
        mainPanel.repaint()
        if (!g_pController.fastRender) {
          val timeToNextFrameStart: Long = (frameStart + millisPerFrame) - System.currentTimeMillis
          if (timeToNextFrameStart > 0) {
            try {
              Thread.sleep(timeToNextFrameStart)
            } catch {
              case e: InterruptedException => throw new RuntimeException(e)
            }
          }
        }
      }
  }
  }

  private def loadInConfigParameters() {
    try {
      CParams.LoadInParameters(Paths.get(Thread.currentThread.getContextClassLoader.getResource("params.ini").toURI))
    } catch {
      case e: URISyntaxException => throw new RuntimeException(e)
    }
  }
}
