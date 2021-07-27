import curses
from argparse import ArgumentParser
from datetime import datetime
from time import sleep

ON = '\N{BLACK CIRCLE}'
OFF = '\N{WHITE CIRCLE}'


class BinaryClock(object):
    
    def __init__(self, time=None, screen=None, decimal=False, military=False):
        """
        A simple object to store time and create a string to display it
        :param screen:  An optional curses screen
        :param time:  An optional time to display, instead of now, as HH:MM:SS
        :param decimal:  Whether or not to display the decimal values next to each line
        :param military:  Whether or not to display in military (00-23) or standard (01-12)
        """
        self.__now = datetime.now()
        self.time = time
        self.__word_template = '{0:0>6b}'
        self.__screen = screen
        self.__decimal = decimal
        self.__military = military

        self.hour = 0
        self.minute = 0
        self.second = 0

        self.update()

    def __str__(self):

        hour = [ON if int(d) else OFF for d in self.__word_template.format(self.hour)]
        minute = [ON if int(d) else OFF for d in self.__word_template.format(self.minute)]
        second = [ON if int(d) else OFF for d in self.__word_template.format(self.second)]

        if self.__decimal:
            hour.append(f'{self.hour:0>2}')
            minute.append(f'{self.minute:0>2}')
            second.append(f'{self.second:0>2}')
        
        hour = f' {" ".join(hour)} '
        minute = f' {" ".join(minute)} '
        second = f' {" ".join(second)} '

        output = '\n'.join([hour, minute, second])
        
        return output

    @property
    def time(self):
        return self.__time
    
    @time.setter
    def time(self, value):

        self.__time = value

        try:
            self.__now = datetime.strptime(value, '%H:%M:%S')
        except (ValueError, TypeError):
            pass

    def update(self):
        
        if self.time is None:
            # only update now if static time is not set
            self.__now = datetime.now()

        if not self.__military:
            if self.__now.hour > 12:
                self.hour = self.__now.hour - 12
            else:
                self.hour = self.__now.hour
        else:
            self.hour = self.__now.hour

        self.minute = self.__now.minute
        self.second = self.__now.second

        if self.__screen is not None:
            self.__screen.clear()
            self.__screen.addstr(self.__str__())
            self.__screen.refresh()

if __name__ == '__main__':

    parser = ArgumentParser()
    parser.add_argument('-d', '--decimal', action='store_true', help='Show decimal values')
    parser.add_argument('-m', '--military', action='store_true', help='Show hours as military (00-23)')
    parser.add_argument('-t', '--time', default=None, help='A time to display as HH:MM:SS')

    args = parser.parse_args()

    screen = curses.initscr()
    curses.curs_set(0)
    c = BinaryClock(screen=screen, time=args.time, decimal=args.decimal, military=args.military)
    c.update()

    while True:
        try:
            sleep(1)
            if args.time is None:
                c.update()
        except KeyboardInterrupt:
            curses.endwin()
            break
