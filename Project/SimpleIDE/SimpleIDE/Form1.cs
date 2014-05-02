using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using System.IO;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        private static DateTime lastTime = new DateTime();
        private static Timer timer = new Timer();
        private static bool textChanged = new bool();
        private static string saveFile = null;
        
        public Form1()
        {
            InitializeComponent();

            timer.Interval = 50;
            timer.Enabled = true;
            timer.Start();
            timer.Tick += new EventHandler(timer_Tick);

            lastTime = DateTime.Now;
            textChanged = false;
        }

        private void timer_Tick(object sender, EventArgs e)
        {
            TimeSpan timeSince = DateTime.Now.Subtract(lastTime);
            if (Convert.ToInt32(timeSince.Seconds) > 4 && richTextBox1.Text != String.Empty && textChanged)
            {
                int loc = richTextBox1.SelectionStart;
                richTextBox1.SelectAll();
                richTextBox1.SelectionFont = new Font(richTextBox1.SelectionFont, FontStyle.Regular);
                richTextBox1.SelectionStart = loc;
                richTextBox1.SelectionLength = 0;
                
                richTextBox1.SaveFile(@"C:\Users\criley1\Documents\!PLDI\PLDITeam\Project\temp\build.txt", RichTextBoxStreamType.PlainText);

                Process process = new Process();
                ProcessStartInfo startInfo = new ProcessStartInfo();
                startInfo.WindowStyle = ProcessWindowStyle.Hidden;
                startInfo.CreateNoWindow = true;
                startInfo.FileName = @"C:\Program Files (x86)\SMLNJ\bin\sml.bat";
                startInfo.Arguments = "IDEMake.sml";
                startInfo.UseShellExecute = false;
                startInfo.RedirectStandardOutput = true;
                startInfo.WorkingDirectory = @"C:\Users\criley1\Documents\!PLDI\PLDITeam\Project";
                process.StartInfo = startInfo;

                process.Start();

                for (int i = 0; i < 17; i++)
                {
                    process.StandardOutput.ReadLine();
                }

                string nextLine = "";
                string output = "";

                while (nextLine != "[New bindings added.]")
                {
                    if (nextLine != "")
                        output += nextLine + "\r\n";

                    nextLine = process.StandardOutput.ReadLine();
                }

                process.Close();

                richTextBox2.Text = output;
                underlineError();
                lastTime = DateTime.Now;
                textChanged = false;
            }
            else if (Convert.ToInt32(timeSince.Seconds) > 4 && richTextBox1.Text == String.Empty && textChanged)
            {
                richTextBox2.Text = "";
            }
        }

        private void underlineError()
        {
            int i = richTextBox2.Find("Evaluation error");
            if (i == -1)
                i = richTextBox2.Find("Parsing error");

            if (i > -1)
            {
                int l = richTextBox2.GetLineFromCharIndex(i);

                foreach (string line in richTextBox1.Lines)
                {
                    if (line == "")
                        l += 1;
                }

                int loc = richTextBox1.SelectionStart;
                richTextBox1.SelectionStart = richTextBox1.GetFirstCharIndexFromLine(l);
                richTextBox1.SelectionLength = richTextBox1.Lines[l].Length;
                richTextBox1.SelectionFont = new Font(richTextBox1.SelectionFont, FontStyle.Underline);
                richTextBox1.SelectionStart = loc;
                richTextBox1.SelectionLength = 0;
                richTextBox1.SelectionFont = new Font(richTextBox1.SelectionFont, FontStyle.Regular);
            }
        }

        private void richTextBox1_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
        {
            lastTime = DateTime.Now;
            textChanged = true;
        }

        private void richTextBox1_TextChanged(object sender, EventArgs e)
        {
            lastTime = DateTime.Now;
            textChanged = true;
        }

        private void richTextBox1_SelectionChanged(object sender, EventArgs e)
        {
            int index = richTextBox1.SelectionStart;
            int line = richTextBox1.GetLineFromCharIndex(index);
            int firstChar = richTextBox1.GetFirstCharIndexFromLine(line);
            int column = index - firstChar;
            toolStripStatusLabel1.Text = "Line " + line.ToString() + ", Column " + column.ToString();
        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog1.ShowDialog();

            try
            {
                if (String.IsNullOrEmpty(openFileDialog1.FileName)) ;       

                else
                {
                    saveFile = openFileDialog1.FileName;
                    if (Path.GetExtension(saveFile) == ".txt")
                        richTextBox1.LoadFile(saveFile, RichTextBoxStreamType.PlainText);
                    else if (Path.GetExtension(saveFile) == ".rtf")
                        richTextBox1.LoadFile(saveFile);
                    else 
                        MessageBox.Show("Please save as a plain text or rtf file.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
            catch (IOException exc)
            {
                MessageBox.Show(exc.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            catch
            {
                MessageBox.Show("An error occured.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (saveFile == null)
                saveAsDialog();
            else 
            {
                if (Path.GetExtension(saveFile) == ".txt")
                {
                    richTextBox1.SaveFile(saveFile, RichTextBoxStreamType.PlainText);
                }
                else
                {
                    richTextBox1.SaveFile(saveFile);
                }
            }
        }

        private void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            saveAsDialog();
        }

        private void saveAsDialog()
        {
            if (saveFileDialog1.ShowDialog() == DialogResult.OK)
            {
                saveFile = saveFileDialog1.FileName;
                if (Path.GetExtension(saveFile) == ".txt")
                    richTextBox1.SaveFile(saveFile, RichTextBoxStreamType.PlainText);
                else if (Path.GetExtension(saveFile) == ".rtf")
                    richTextBox1.SaveFile(saveFile);
                else
                    MessageBox.Show("Please save as a plain text or rtf file.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void viewHelpToolStripMenuItem_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Type your program into the top textbox. After a few seconds of no typing, your program will automatically be checked and any resulting messages will be displayed in the bottom textbox.", "Help", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Close();
        }

    }
}
