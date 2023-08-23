import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def line_fit_plot(x, y, title, xlabel, ylabel):
    # Ajuste linear usando polyfit
    coeffs = np.polyfit(x, y, 1)
    m, b = coeffs

    # Criar os valores de y ajustados usando os coeficientes
    y_fit = m * x + b

    # Plotar os pontos originais e a linha de ajuste
    plt.scatter(x, y, label="Dados Originais")
    plt.plot(x, y_fit, color='black', label="Ajuste de Linha")

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend()
    plt.grid(True)
    plt.show()

def main():
    # Substitua 'caminho/do/arquivo.csv' pelo caminho do seu arquivo CSV
    arquivo_csv = 'csv/SimpleQueue.csv'

    variables_info = [
        ['alloc', 'energy_dram', 'Total Allocation (MB)', 'Energy DRAM (J)'],
        ['energy_pkg', 'time', 'Energy PKG (J)', 'Time (s)'],
        ['energy_pkg', 'energy_dram', 'Energy PKG (J)', 'Energy DRAM (J)'],
        ['energy_dram', 'time', 'Energy DRAM (J)', 'Time (s)'],
    ]

    variables_info = variables_info[0:1]

    for variable_info in variables_info:
        for row in range(0,10):

            [x_var, y_var, x_label, y_label] = variable_info

            # Leitura do arquivo CSV para um DataFrame
            df = pd.read_csv(arquivo_csv)

            #df = df.drop([row,row])

            # Filtrar apenas as colunas das variáveis escolhidas
            df_selected = df[[x_var, y_var]]

            # Calcular a correlação de Pearson
            correlacao = df_selected.corr().iloc[0, 1]

            #print(f"A correlação de Pearson entre {x_var} e {y_var} é: {correlacao:.9f}")

            print(f"{correlacao:.2f}")

            # Plotar o gráfico de linha com o ajuste de linha
            line_fit_plot(df_selected[x_var], df_selected[y_var], f'Gráfico de Linha - {x_var} vs {y_var}', x_label, y_label)
            break

if __name__ == "__main__":
    main()
