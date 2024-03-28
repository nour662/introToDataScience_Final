import airflow 
from airflow import PythonOperator 
from datetime import timedelta, datetime

default_args = {
        'owner' : 'airflow',
        'start_date' : datetime(2024, 1,1 )
}2