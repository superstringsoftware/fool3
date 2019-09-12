using System;
using System.Drawing;
using System.Windows.Forms;

class MForm : Form
{
    public MForm()
    {
        Text = "Button";
        Size = new Size(250, 200);

        Button button = new Button();

        button.Location = new Point(30, 20);
        button.Text = "Quit";
        button.Click += new EventHandler(OnClick);
        button.MouseEnter += new EventHandler(OnEnter);

        Controls.Add(button);
        CenterToScreen();
    }

    void OnClick(object sender, EventArgs e)
    {
        Close();
    }

    void OnEnter(object sender, EventArgs e)
    {
        Console.WriteLine("Button Entered");
    }

}

class MApplication
{
    public static void Main()
    {
        Application.Run(new MForm());
    }
}