using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using System.Windows.Input;

namespace FSchemeFrontend
{
    class MainWindowViewModel : INotifyPropertyChanged
    {
        private ICommand m_CompileCommand;
        public ICommand CompileCommand
        {
            get
            {
                if (m_CompileCommand == null)
                {
                    m_CompileCommand = new RelayCommand(
                        o => Compile(SourceCodeText));
                }

                return m_CompileCommand;
            }
        }

        private ICommand m_RecalculateCommand;
        public ICommand RecalculateCommand
        {
            get
            {
                if (m_RecalculateCommand == null)
                {
                    m_RecalculateCommand = new RelayCommand(
                        o => RecalculateResults(SourceCodeText));
                }

                return m_RecalculateCommand;
            }
        }

        private ICommand m_LoadTestsCommand;
        public ICommand LoadTestsCommand
        {
            get
            {
                if (m_LoadTestsCommand == null)
                {
                    m_LoadTestsCommand = new RelayCommand(
                        o => LoadTests());
                }

                return m_LoadTestsCommand;
            }
        }

        private ICommand m_InferTypesCommand;
        public ICommand InferTypesCommand
        {
            get
            {
                if (m_InferTypesCommand == null)
                {
                    m_InferTypesCommand = new RelayCommand(
                        o => AnalyzeTypes(SourceCodeText));
                }

                return m_InferTypesCommand;
            }
        }

        private string m_SourceCodeText;
        public string SourceCodeText
        {
            get { return m_SourceCodeText; }
            set
            {
                if (m_SourceCodeText != value)
                {
                    m_SourceCodeText = value;
                    OnPropertyChanged("SourceCodeText");
                }
            }
        }

        private string m_Results;
        public string Results
        {
            get { return m_Results; }
            set
            {
                if (m_Results != value)
                {
                    m_Results = value;
                    OnPropertyChanged("Results");
                }
            }
        }

        private void RecalculateResults(string newCode)
        {
            Results = FScheme.Interpreter.GetProgramText(newCode);
        }

        private void Compile(string newCode)
        {
            Results = FScheme.Compiler.RunProgram(newCode);
        }

        private void AnalyzeTypes(string newCode)
        {
            Results = FScheme.DamasMilner.AnalyzeTypes(newCode);
        }

        private void LoadTests()
        {
            SourceCodeText = FScheme.Tests.GetTestSourceCode();
        }

        private void OnPropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }

        public event PropertyChangedEventHandler PropertyChanged;
    }
}
