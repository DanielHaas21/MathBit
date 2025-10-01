'use client';
import * as React from 'react';
import { InputBase } from './InputBase';
import { DayPicker, DateRange } from 'react-day-picker';
import { Popover, PopoverButton, PopoverPanel, Transition } from '@headlessui/react';
import { Icon } from './Icon';
import { cs } from 'date-fns/locale';
import 'react-day-picker/dist/style.css';
import { RangeDate } from '../types/RangeDate';
import { parse, isValid } from 'date-fns';
import { cn } from '../utils';
import { GLOBAL_HEIGHT } from '../types/global-size';

// This file is lightly documented for the sake of its complexity

export interface DatepickerProps {
  mode: 'single' | 'range';
  withPresets?: boolean;
  withConfirmButtons?: boolean;
  MonthRange?: 1 | 2 | 4 | 6 | 12;
  value?: RangeDate | Date;
  onChange?: (value: Date | RangeDate) => void;
  closeOnConfirm?: boolean;
  onBlur?: (event: React.FocusEvent<HTMLInputElement>) => void;
  className?: string;
  placeholder?: string;
  hasError?: boolean;
  triggerAnchor?: 'left' | 'right';
  disabled?: boolean;
  disableBefore?: Date;
  disableAfter?: Date;
  height?: GLOBAL_HEIGHT;
}

export const Datepicker = React.forwardRef<HTMLInputElement, DatepickerProps>(
  (
    {
      mode,
      MonthRange,
      withPresets = false,
      onChange,
      withConfirmButtons = false,
      value,
      closeOnConfirm = true,
      className,
      hasError,
      disabled,
      onBlur,
      height = 'md',
      triggerAnchor = 'left',
      disableAfter,
      disableBefore,
      placeholder
    },
    ref
  ) => {
    /*
    Helpers
   */
    const isRangeDate = (val: any): val is RangeDate =>
      val && typeof val === 'object' && ('from' in val || 'to' in val);

    const isDateInRange = (date: Date, range: RangeDate) => {
      if (!range.from || !range.to) return false;
      return date >= range.from && date <= range.to;
    };

    const asDateRange = (range: RangeDate) => {
      return range.from ? (range as DateRange) : undefined;
    };
    /*
      States
    */

    // normal mode states
    const [singleDate, setSingleDate] = React.useState<Date | undefined>();
    const [rangeDate, setRangeDate] = React.useState<RangeDate>({});

    // confirmation button states
    const [pendingSingleDate, setPendingSingleDate] = React.useState<Date | undefined>();
    const [pendingRangeDate, setPendingRangeDate] = React.useState<RangeDate>({});

    // determines how many months show
    const MonthRangeConfig = MonthRange ? MonthRange : mode === 'range' ? 2 : 1;

    // input value, ignores confirmation/normal states, works in both cases
    const [rawInputValue, setRawInputValue] = React.useState<string>('');
    // confirmation state display near the buttons
    const [displayValue, setDisplayValue] = React.useState<string>('');

    //confirm close forcing
    const popoverButtonRef = React.useRef<HTMLButtonElement>(null);

    const handleConfirm = () => {
      if (popoverButtonRef.current) {
        popoverButtonRef.current.click();
      }
    };
    /*
      Switching between confirmation and range
    */
    React.useEffect(() => {
      setPendingSingleDate(undefined);
      setPendingRangeDate({});
    }, [withConfirmButtons]);

    /*
    Initial value 
    */
    React.useEffect(() => {
      if (!value) return;

      if (mode === 'single') {
        /*
        Single value
        */
        if (value instanceof Date) {
          if (withConfirmButtons) {
            setPendingSingleDate(value);
            return;
          }

          setSingleDate(value);
          setRawInputValue(value.toLocaleDateString('cs-CZ'));
        } else if (isRangeDate(value) && value.from) {
          /*
            Object passed to single mode date
          */
          if (withConfirmButtons) {
            setPendingSingleDate(value.from);
            return;
          }

          setSingleDate(value.from);
          setRawInputValue(value.from.toLocaleDateString('cs-CZ'));
        }
      } else if (mode === 'range') {
        /*
            Range value
          */
        if (isRangeDate(value)) {
          if (withConfirmButtons) {
            setPendingRangeDate(value);
            return;
          }

          setRangeDate(value);

          const formattedValue = value.to // value.from cant be undefined because of this isRangeDate(value)
            ? `${value.from!.toLocaleDateString('cs-CZ')} - ${value.to.toLocaleDateString('cs-CZ')}`
            : `${value.from!.toLocaleDateString('cs-CZ')}`;

          setRawInputValue(formattedValue);
        } else if (value instanceof Date) {
          /*
            A Date passed to range mode, its casted as an object where from and to are the same
          */
          if (withConfirmButtons) {
            setPendingRangeDate({ from: value, to: value });
            return;
          }

          setRangeDate({ from: value, to: value });
          const formattedValue = `${value.toLocaleDateString('cs-CZ')} - ${value.toLocaleDateString('cs-CZ')}`;

          setRawInputValue(formattedValue);
        }
      }
    }, [value, mode]);

    /*
      Switching between modes
      switching between confirmation and normal can transfer values 
    */
    React.useEffect(() => {
      if (mode === 'single') {
        if (pendingRangeDate.from) {
          setSingleDate(pendingRangeDate.from);
          return;
        }

        if (rangeDate.from) {
          setSingleDate(rangeDate.from);
          setRawInputValue(rangeDate.from.toLocaleDateString('cs-CZ'));
          withConfirmButtons && setDisplayValue(rangeDate.from.toLocaleDateString('cs-CZ'));
        }
      } else if (mode === 'range') {
        if (pendingSingleDate) {
          setRangeDate({ from: pendingSingleDate, to: pendingSingleDate });
          return;
        }

        if (singleDate) {
          setRangeDate({ from: singleDate, to: singleDate });
          setRawInputValue(
            `${singleDate.toLocaleDateString('cs-CZ')} - ${singleDate.toLocaleDateString('cs-CZ')}`
          );
          withConfirmButtons &&
            setDisplayValue(
              `${singleDate.toLocaleDateString('cs-CZ')} - ${singleDate.toLocaleDateString('cs-CZ')}`
            );
        }
      }
    }, [mode]);
    // Wrapper to trigger onBlur
    const containerRef = React.useRef<HTMLDivElement>(null);

    const handleBlur = () => {
      onBlur?.({} as React.FocusEvent<HTMLInputElement>);
    };

    // Close detection: if click outside input & popover, trigger onBlur
    React.useEffect(() => {
      function handleClickOutside(event: MouseEvent) {
        if (containerRef.current && !containerRef.current.contains(event.target as Node)) {
          handleBlur();
        }
      }

      document.addEventListener('mousedown', handleClickOutside);
      return () => {
        document.removeEventListener('mousedown', handleClickOutside);
      };
    }, []);
    /*
      Presets - all are in (ms)
    */
    const presetRanges = [
      { label: 'Today', range: { from: new Date(), to: new Date() } },
      {
        label: 'Yesterday',
        range: {
          from: new Date(Date.now() - 24 * 60 * 60 * 1000),
          to: new Date(Date.now() - 24 * 60 * 60 * 1000)
        }
      },
      {
        label: 'Last 7 Days',
        range: { from: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000), to: new Date() }
      },
      {
        label: 'Last 30 Days',
        range: { from: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000), to: new Date() }
      },
      {
        label: 'This Month',
        range: {
          from: new Date(new Date().getFullYear(), new Date().getMonth(), 1),
          to: new Date()
        }
      },
      {
        label: 'Last Month',
        range: {
          from: new Date(new Date().getFullYear(), new Date().getMonth() - 1, 1),
          to: new Date(new Date().getFullYear(), new Date().getMonth(), 0)
        }
      }
    ];

    /*
      Custom style modifiers 
      index wise weeks end on saturday hence index[6], lastdayInWeek has the first index, monday second 
      All modifiers must have variants for both normal or pending dates, they wont apply if they dont
    */
    const modifiers = React.useMemo(() => {
      if (mode === 'single') {
        return {};
      }

      return {
        rangeStart: withConfirmButtons
          ? pendingRangeDate.from
            ? pendingRangeDate.from
            : rangeDate.from
          : rangeDate.from,
        rangeEnd: withConfirmButtons
          ? pendingRangeDate.to
            ? pendingRangeDate.to
            : rangeDate.to
          : rangeDate.to,
        firstInWeek: (date: Date) =>
          withConfirmButtons
            ? pendingRangeDate.from || pendingRangeDate.to
              ? isDateInRange(date, pendingRangeDate) && date.getDay() === 1
              : isDateInRange(date, rangeDate) && date.getDay() === 1
            : isDateInRange(date, rangeDate) && date.getDay() === 1,
        lastInWeek: (date: Date) =>
          withConfirmButtons
            ? pendingRangeDate.from || pendingRangeDate.to
              ? isDateInRange(date, pendingRangeDate) && date.getDay() === 0
              : isDateInRange(date, rangeDate) && date.getDay() === 0
            : isDateInRange(date, rangeDate) && date.getDay() === 0
      };
    }, [mode, withConfirmButtons, pendingRangeDate, rangeDate]);

    /*
      Common props for DayPicker both modes, and styling for modifiers, month range application
    */
    const commonDayPickerProps = {
      className: 'm-[12px]',
      modifiers,
      modifiersClassNames: {
        selected:
          'bg-link-hover  hover:!outline-none text-text-white !text-lg' +
          (mode === 'single' ? ' rounded-full' : ' hover:rounded-none mt-1'),
        hover: 'rounded-full border border-link-hover',
        range_middle:
          '!bg-white-200 hover:!outline-none hover:rounded-none !text-text-black hover:!text-black-700 ',
        firstInWeek: 'rounded-l-full hover:!rounded-l-full',
        lastInWeek: 'rounded-r-full hover:!rounded-r-full',
        rangeStart: 'rounded-l-full !bg-white-200 hover:!rounded-l-full',
        rangeEnd: 'rounded-r-full bg-white-200 hover:!rounded-r-full'
      },
      classNames: {
        day: 'hover:outline hover:outline-link-base hover:rounded-full',
        table: 'border-separate border-spacing-y-1',
        months:
          MonthRangeConfig === 12
            ? 'grid grid-cols-4 gap-4'
            : MonthRangeConfig === 6
              ? 'grid grid-cols-3 gap-4'
              : MonthRangeConfig === 4
                ? 'grid grid-cols-2 gap-6'
                : 'flex gap-6' // fallback for 1-2
      },
      numberOfMonths: MonthRangeConfig,
      locale: cs,
      showOutsideDays: true
    };

    return (
      <Popover>
        {/*Date input and click  section*/}
        <InputBase
          disabled={disabled}
          value={rawInputValue}
          className={cn(className, GLOBAL_HEIGHT[height])}
          placeholder={placeholder}
          hasError={hasError}
          onBlur={handleBlur}
          onChange={(e) => {
            /*
                This entire block controls the input mechanism
              */

            const value = e.target.value;

            /*
                If the value is empty we also empty out all states and trigger a change 
              */
            if (value === '') {
              withConfirmButtons
                ? mode === 'range'
                  ? setPendingRangeDate({})
                  : setPendingSingleDate(undefined)
                : mode === 'range'
                  ? setRangeDate({})
                  : setSingleDate(undefined);

              if (mode === 'range') {
                setRangeDate({});
                onChange?.({});
              } else {
                setSingleDate(undefined);
                onChange?.(undefined!);
              }
            }

            setRawInputValue(value);

            const cleanValue = value.replace(/\s/g, '').replace(/\u200B/g, '');

            /*
                In both of these a pattern is first tested if it passes a date is set 
                This ensures that if theres an invalid date, the last valid date well stored as value or nothing if there isnt one
                This block contains a lot of pyramids but it couldnt be helped, and if it works dont touch it!
              */
            if (mode === 'single') {
              const singlePattern = /^\d{1,2}\.\d{1,2}\.\d{4}$/;
              if (singlePattern.test(cleanValue)) {
                const parsedDate = parse(cleanValue, 'd.M.yyyy', new Date());

                if (isValid(parsedDate)) {
                  if (withConfirmButtons) {
                    setPendingSingleDate(parsedDate);
                  } else {
                    setSingleDate(parsedDate);
                    onChange?.(parsedDate);
                  }
                }
              }
            }

            if (mode === 'range') {
              const normalized = cleanValue.replace(/\//g, '.');
              const parts = normalized.split('-');

              if (parts.length === 2) {
                const [start, end] = parts.map((p) => p.trim());
                const datePattern = /^\d{1,2}\.\d{1,2}\.\d{4}$/;

                if (start && end && datePattern.test(start) && datePattern.test(end)) {
                  const fromDate = parse(start, 'd.M.yyyy', new Date());
                  const toDate = parse(end, 'd.M.yyyy', new Date());

                  if (isValid(fromDate) && isValid(toDate) && fromDate <= toDate) {
                    const newRange = { from: fromDate, to: toDate };

                    if (withConfirmButtons) {
                      setPendingRangeDate(newRange);
                    } else {
                      setRangeDate(newRange);
                      onChange?.(newRange);
                    }
                  }
                }
              }
            }
          }}
          onKeyDown={(e) => {
            const allowedKeys = [
              'Backspace',
              'Delete',
              'ArrowLeft',
              'ArrowRight',
              'Tab',
              ' ',
              '-',
              '.',
              '/'
            ];
            if (!/[0-9]/.test(e.key) && !allowedKeys.includes(e.key)) {
              e.preventDefault();
            }
          }}
          leftContent={
            triggerAnchor === 'left' &&
            !disabled && (
              <PopoverButton
                ref={popoverButtonRef}
                onClick={() => onBlur?.({} as unknown as React.FocusEvent<HTMLInputElement>)}
                className="flex items-center p-1 rounded focus-visible:!border-none focus-visible:!outline-none focus-visible:!ring-none"
              >
                <Icon name="calendar-clock" />
              </PopoverButton>
            )
          }
          rightContent={
            triggerAnchor === 'right' &&
            !disabled && (
              <PopoverButton
                ref={popoverButtonRef}
                onClick={() => onBlur?.({} as unknown as React.FocusEvent<HTMLInputElement>)}
                className="flex items-center p-1 rounded focus-visible:!border-none focus-visible:!outline-none focus-visible:!ring-none"
              >
                <Icon name="calendar-clock" />
              </PopoverButton>
            )
          }
          ref={ref}
        />

        <Transition
          enter="transition ease-out duration-200"
          enterFrom="opacity-0 translate-y-1"
          enterTo="opacity-100 translate-y-0"
          leave="transition ease-in duration-150"
          leaveFrom="opacity-100 translate-y-0"
          leaveTo="opacity-0 translate-y-1"
        >
          <PopoverPanel className="mt-2 border border-white-200 rounded-lg shadow-lg bg-white-50 w-max flex flex-col overflow-hidden absolute z-[51]">
            <div className="flex flex-row">
              {/*Presets section*/}
              {withPresets && mode === 'range' && (
                <div className="flex flex-col gap-3 pt-8 ps-5 pr-5 bg-white-200 border-r border-r-white-800">
                  {presetRanges.map((preset) => (
                    <button
                      key={preset.label}
                      onClick={() => {
                        /*
                          Onchange is triggered when set
                        */
                        if (withConfirmButtons) {
                          setPendingRangeDate(preset.range);
                        } else {
                          setRangeDate(preset.range);
                          onChange?.(preset.range);
                          const formattedValue = `${preset.range.from!.toLocaleDateString('cs-CZ')} - ${preset.range.to.toLocaleDateString('cs-CZ')}`;
                          setRawInputValue(formattedValue);
                        }
                      }}
                      className="text-[16px] text-left text-text-black hover:text-black-700 transition duration:300"
                    >
                      {preset.label}
                    </button>
                  ))}
                </div>
              )}

              {mode === 'single' ? (
                <DayPicker
                  disabled={[
                    ...(disableBefore ? [{ before: disableBefore }] : []),
                    ...(disableAfter ? [{ after: disableAfter }] : [])
                  ]}
                  {...commonDayPickerProps}
                  mode="single"
                  selected={
                    withConfirmButtons
                      ? pendingSingleDate
                        ? pendingSingleDate
                        : singleDate
                      : singleDate
                  }
                  onSelect={(date) => {
                    if (withConfirmButtons) {
                      setPendingSingleDate(date as Date);
                    } else {
                      setSingleDate(date as Date);
                      setRawInputValue(date ? date.toLocaleDateString('cs-CZ') : '');
                      onChange?.(date as Date);
                    }
                  }}
                />
              ) : (
                <DayPicker
                  {...commonDayPickerProps}
                  mode="range"
                  selected={
                    withConfirmButtons
                      ? pendingRangeDate.from || pendingRangeDate.to
                        ? asDateRange(pendingRangeDate)
                        : asDateRange(rangeDate)
                      : asDateRange(rangeDate)
                  }
                  onSelect={(value) => {
                    if (withConfirmButtons) {
                      if (!value || !value.from) {
                        setRangeDate({});
                        setPendingRangeDate({});
                        setRawInputValue('');
                        setDisplayValue('');
                        onChange?.({});
                      } else {
                        setPendingRangeDate(value as RangeDate);
                      }
                    } else {
                      if (!value || !value.from) {
                        setRangeDate({});
                        setRawInputValue('');
                        onChange?.({});
                      } else {
                        const fromDate = value.from;
                        const toDate = value.to;

                        const formattedValue = toDate
                          ? `${fromDate.toLocaleDateString('cs-CZ')} - ${toDate.toLocaleDateString('cs-CZ')}`
                          : `${fromDate.toLocaleDateString('cs-CZ')}`;

                        setRangeDate(value as RangeDate);
                        setRawInputValue(formattedValue);
                        onChange?.(value as RangeDate);
                      }
                    }
                  }}
                  required={false}
                />
              )}
            </div>
            {/*Confirmation section*/}
            {withConfirmButtons && (
              <div className="flex justify-end items-center gap-2 p-3 border-t border-t-white-800">
                <p className="text-black-500 mr-2">{displayValue}</p>
                <button
                  onClick={() => {
                    if (mode === 'single') {
                      if (rawInputValue !== '' && singleDate === undefined) {
                        setRawInputValue('');
                        setPendingSingleDate(undefined);
                        if (closeOnConfirm) handleConfirm();
                        return;
                      }

                      if (pendingSingleDate) {
                        setPendingSingleDate(undefined);
                        if (closeOnConfirm && !singleDate) handleConfirm();
                      } else if (singleDate) {
                        setSingleDate(undefined);
                        setRawInputValue('');
                        setDisplayValue('');
                        if (closeOnConfirm) handleConfirm();
                      }
                    } else {
                      if ((rawInputValue !== '' && !rangeDate.from) || !rangeDate.to) {
                        setRawInputValue('');
                        setPendingRangeDate({});
                        if (closeOnConfirm) handleConfirm();
                        return;
                      }

                      if (pendingRangeDate.from || pendingRangeDate.to) {
                        setPendingRangeDate({});
                      } else if (rangeDate.from || rangeDate.to) {
                        setRangeDate({});
                        setRawInputValue('');
                        setDisplayValue('');
                        if (closeOnConfirm) handleConfirm();
                      }
                    }
                  }}
                  className="px-2 py-1 font-medium text-sm border rounded-md text-text-black border-2 border-white-800"
                >
                  Zrušit
                </button>
                <button
                  onClick={() => {
                    if (mode === 'single') {
                      if (pendingSingleDate) setSingleDate(pendingSingleDate);
                      setRawInputValue(
                        pendingSingleDate
                          ? pendingSingleDate.toLocaleDateString('cs-CZ')
                          : singleDate
                            ? singleDate.toLocaleDateString('cs-CZ')
                            : ''
                      );
                      setDisplayValue(
                        pendingSingleDate
                          ? pendingSingleDate.toLocaleDateString('cs-CZ')
                          : singleDate
                            ? singleDate.toLocaleDateString('cs-CZ')
                            : ''
                      );

                      onChange?.(pendingSingleDate as Date);
                      setPendingSingleDate(undefined);
                    } else {
                      if (pendingRangeDate.from || pendingRangeDate.to)
                        setRangeDate(pendingRangeDate);
                      const fromDate = pendingRangeDate.from
                        ? pendingRangeDate.from
                        : rangeDate.from;
                      const toDate = pendingRangeDate.to ? pendingRangeDate.to : rangeDate.to;

                      const formattedValue = fromDate
                        ? toDate
                          ? `${fromDate.toLocaleDateString('cs-CZ')} - ${toDate.toLocaleDateString('cs-CZ')}`
                          : `${fromDate.toLocaleDateString('cs-CZ')}`
                        : '';

                      setRawInputValue(formattedValue);
                      setDisplayValue(formattedValue);
                      onChange?.(pendingRangeDate);
                      setPendingRangeDate({});
                    }
                    if (closeOnConfirm) handleConfirm();
                  }}
                  className="px-2 py-1 text-sm text-text-white border rounded-md bg-link-hover"
                >
                  Potvrdit
                </button>
              </div>
            )}
          </PopoverPanel>
        </Transition>
      </Popover>
    );
  }
);

Datepicker.displayName = 'Datepicker';
