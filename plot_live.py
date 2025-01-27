import pandas as pd
import matplotlib.pyplot as plt
from scipy.signal import savgol_filter
import time

file_path = 'accuracy_log.txt'

plt.ion()  
fig, ax = plt.subplots(figsize=(10, 6))

def read_accuracies(file_path):
    with open(file_path, 'r') as file:
        return [float(line.strip()) for line in file]

def update_plot():
    accuracies = read_accuracies(file_path)
    anzahl_werte = [(i + 1) * 50 for i in range(len(accuracies))]
    
    if len(accuracies) >= 5: 
        smoothed_accuracies = savgol_filter(accuracies, window_length=5, polyorder=2)
    else:
        smoothed_accuracies = accuracies  
    
    ax.clear() 
    ax.plot(anzahl_werte, smoothed_accuracies, linestyle='-', color='r', label='Genauigkeit')
    
    ax.set_title('Genauigkeit vs. Anzahl Werte', fontsize=16)
    ax.set_xlabel('Anzahl Werte', fontsize=14)
    ax.set_ylabel('Genauigkeit', fontsize=14)
    ax.grid(True, linestyle='--', alpha=0.7)
    ax.legend(fontsize=12)
    ax.tick_params(axis='both', labelsize=12)
    plt.tight_layout()
    plt.draw() 
    plt.pause(0.1) 

try:
    while True:
        update_plot()  
        time.sleep(1) 
except KeyboardInterrupt:
    print("Live-Plotting beendet.")
    plt.ioff()  
    plt.show()  
