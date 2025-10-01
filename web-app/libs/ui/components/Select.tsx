import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { SelectInputOption } from '../types/SelectInputOption';
import { Field, Label } from '@headlessui/react';
import { Listbox, ListboxOption, Transition } from '@headlessui/react';
import { IconName } from '../icons/names';
import { Icon } from './Icon';
import { GLOBAL_HEIGHT } from '../types/global-size';

type BaseSelectInputAttributes = Pick<
  React.SelectHTMLAttributes<HTMLSelectElement>,
  'id' | 'className' | 'multiple' | 'disabled' | 'required' | 'autoFocus' | 'onBlur'
>;

const SelectInputVariants = cva(
  ` flex items-center p-2 gap-3 border rounded-lg outline-none transition`,
  {
    variants: {
      size: {
        md: 'w-[320px]',
        lg: 'w-[400px]',
        full: 'w-full'
      },
      variant: {
        default: '!bg-background-inputs !border-white-950',
        white: '!bg-white-50 !border-white-950'
      },
      hasError: {
        true: '!border-error-text',
        false: ''
      }
    },
    defaultVariants: {
      size: 'md',
      variant: 'default',
      hasError: false
    }
  }
);

type SelectInputVariantProps = VariantProps<typeof SelectInputVariants>;

export interface SelectInputProps extends SelectInputVariantProps, BaseSelectInputAttributes {
  className?: string;
  labelText?: string;
  options: SelectInputOption[];
  buttonIcon?: IconName;
  buttonLabel?: string;
  isMulti?: boolean;
  isSearchable?: boolean;
  onChange?: (value: any | any[]) => void;
  onSearch?: (value: string) => void;
  UnselectedOption?: string;
  value?: any | any[];
  height?: GLOBAL_HEIGHT;
}

/**
 * @param {string} [className]
 * @param {string} [labelText] Optional label text above the select.
 * @param {boolean} [isSearchable] Whether the select input allows searching.
 * @param {boolean} [isMulti] Whether the select input allows multi-select.
 * @param {SelectInputOption[]} options The available options for the select, required.
 * @param {(value: string | string[]) => void} [onChange] Lets you expose the selected values. Note that only the option.value will be passed as value not the entire option object!
 * @param {(value: string) => void} [onSearch] Lets you expose the searched term
 * @param {SelectInputOption | SelectInputOption[]} [value] lets you input a value.
 * @param {string} [UnselectedOption] Creates an extra options that has an undefined value, this param only provides the label
 */
export const Select = React.forwardRef<HTMLSelectElement, SelectInputProps>(
  (
    {
      className,
      labelText,
      isSearchable,
      options,
      isMulti,
      value,
      disabled,
      onChange,
      onSearch,
      onBlur,
      buttonLabel,
      buttonIcon,
      hasError = false,
      UnselectedOption,
      variant,
      size,
      height = 'md',
      ...props
    },
    ref
  ) => {
    //In case the user puts a default value, its checked if values are in the options or if a disbaled options is inititally selected

    const mergedOptions = React.useMemo(() => {
      if (UnselectedOption && !isMulti) {
        return [{ value: '', label: UnselectedOption }, ...options];
      }
      return options;
    }, [UnselectedOption, options]);

    const initialSelected = React.useMemo(() => {
      const list = isMulti ? options : mergedOptions;

      if (isMulti) {
        if (Array.isArray(value)) {
          return list.filter((option) => value.includes(option.value));
        }
        return [];
      } else {
        const selected = list.find((option) => option.value === value);
        return selected ? [selected] : [];
      }
    }, [value, options, mergedOptions, isMulti]);

    const [selectedValues, setSelectedValues] =
      React.useState<SelectInputOption[]>(initialSelected);
    const [inputValue, setInputValue] = React.useState(''); // raw input
    const [query, setQuery] = React.useState(''); // debounced value
    const [open, isOpen] = React.useState<boolean>(false);
    const inputRef = React.useRef<HTMLInputElement>(null);
    const containerRef = React.useRef<HTMLDivElement>(null);

    React.useEffect(() => {
      const handler = setTimeout(() => {
        if (inputValue === query) return;
        setQuery(inputValue);
        onSearch?.(inputValue);
      }, 300);

      return () => clearTimeout(handler);
    }, [inputValue, 300, onSearch]);

    React.useEffect(() => {
      function handleClickOutside(event: MouseEvent) {
        if (containerRef.current && !containerRef.current.contains(event.target as Node)) {
          isOpen(false);
        }
      }

      document.addEventListener('mousedown', handleClickOutside);
      return () => {
        document.removeEventListener('mousedown', handleClickOutside);
      };
    }, []);
    React.useEffect(() => {
      const node = containerRef.current;
      if (!node) return;

      const handleBlur = (event: FocusEvent) => {
        if (node && !node.contains(event.relatedTarget as Node)) {
          onBlur?.(event as any);
        }
      };

      node.addEventListener('focusout', handleBlur);
      return () => node.removeEventListener('focusout', handleBlur);
    }, [onBlur]);
    // Select handling
    const handleSelect = (option: SelectInputOption) => {
      if (isMulti) {
        const exists = selectedValues.find((o) => o.value === option.value);

        const newValues = exists
          ? selectedValues.filter((o) => o.value !== option.value)
          : [...selectedValues, option];

        setSelectedValues(newValues);
        onChange?.(newValues.map((val) => val.value));
      } else {
        setSelectedValues([option]);
        onChange?.(option.value);
      }
    };

    React.useEffect(() => {
      if (isMulti) {
        if (Array.isArray(value)) {
          setSelectedValues(mergedOptions.filter((option) => value.includes(option.value)));
        } else {
          setSelectedValues([]);
        }
      } else {
        const selected = Array.isArray(value)
          ? mergedOptions.find((option) => value.includes(option.value))
          : mergedOptions.find((option) => option.value === value);

        setSelectedValues(selected ? [selected] : []);
      }
    }, [value, mergedOptions, isMulti]);

    // Search filtering, returns matching on top and non-matching under the former, therefore the select never feels 'empty'
    const filteredOptions = React.useMemo(() => {
      if (!isSearchable) return mergedOptions;

      return [
        ...mergedOptions.filter((option) =>
          option.label.toLowerCase().includes(query.toLowerCase())
        ),
        ...mergedOptions.filter(
          (option) => !option.label.toLowerCase().includes(query.toLowerCase())
        )
      ];
    }, [mergedOptions, query, isSearchable]);

    //Highlighting matching parts of the search query, highlighting has a text shadow
    const highlightMatch = (label: string, query: string) => {
      if (!query) return label;

      const regex = new RegExp(`(${query})`, 'i');
      const parts = label.split(regex);

      return parts.map((part, i) =>
        regex.test(part) ? (
          <span
            key={i}
            className="text-brand-blue-700"
            style={{ textShadow: '0 0 1px #C2DFF7, 0 0 5px #C2DFF7' }}
          >
            {part}
          </span>
        ) : (
          part
        )
      );
    };

    //Removes the selected value when using multi-select
    const RemoveSelectedItem = (value: string) => {
      isOpen(true);
      setSelectedValues(selectedValues.filter((o) => o.value !== value));
      isOpen(true);
    };

    //dropdown size
    const DropdownSize = {
      md: 'w-[315px]',
      lg: 'w-[395px]',
      full: ''
    }[size ?? 'md'];

    return (
      <div ref={containerRef}>
        <Field className={cn('flex flex-col gap-2', size == 'full' ? 'w-full' : 'w-fit')}>
          {labelText && <Label>{labelText}</Label>}
          <Listbox as="div" ref={ref} {...props}>
            <div
              role="button"
              tabIndex={0}
              onClick={() => {
                if (disabled) return;
                if (!open) {
                  onBlur?.({} as unknown as React.FocusEvent<HTMLSelectElement>);
                }
                isOpen((prev) => !prev);
              }}
              className={cn(
                SelectInputVariants({ size, variant, hasError }),
                GLOBAL_HEIGHT[height],
                className,
                'inline-flex  gap-1 relative',
                open && 'ring-4 ring-brand-blue-100 !border-brand-blue-400 pr-6',
                open && hasError && 'ring-4 ring-error-bg !border-error-text',
                isMulti && 'flex-wrap'
              )}
            >
              {/* Shows the selected icon in the case of a non-search non-multi select*/}
              {(() => {
                const iconName = !isMulti
                  ? !isSearchable
                    ? selectedValues?.[0]?.icon || buttonIcon
                    : buttonIcon
                  : buttonIcon;
                return iconName ? <Icon name={iconName} /> : null;
              })()}

              {/* Renders small tags in case of multi or searchable select, multi selects also have a remove button */}
              {isMulti ? (
                selectedValues.map((item) => (
                  <span
                    key={item.value}
                    className="ms-1 mr-1 group relative inline-flex items-center cursor-default bg-brand-blue-50 text-primary-base rounded-lg px-2 py-0.5 text-sm select-none transition-all duration-300"
                  >
                    {item.icon && <Icon name={item.icon} className="mr-1"></Icon>}

                    {/* Label shifts left when icon appears */}
                    <span className="transition-transform duration-300 ease-in-out ms-4 group-hover:-translate-x-3">
                      {item.label}
                    </span>

                    {/* Close icon container */}
                    <span className="flex items-center overflow-hidden w-4 ml-1">
                      <Icon
                        name="xmark"
                        className="h-4 w-4 cursor-pointer text-primary-base opacity-0 group-hover:opacity-100 transition-opacity duration-300"
                        onClick={(e) => {
                          e.stopPropagation();
                          RemoveSelectedItem(item.value);
                        }}
                      />
                    </span>
                  </span>
                ))
              ) : isSearchable &&
                selectedValues.length > 0 &&
                !inputValue &&
                selectedValues[0]?.icon ? (
                <Icon name={selectedValues[0]?.icon}></Icon>
              ) : (
                ''
              )}

              {/* Shows either the selected value or an input if searchable, the selected values are then moved into tags */}
              {isSearchable ? (
                <input
                  ref={inputRef}
                  type="text"
                  disabled={disabled}
                  value={inputValue}
                  onChange={(e) => setInputValue(e.target.value)}
                  onClick={(e) => {
                    e.stopPropagation();
                    isOpen(true);
                  }}
                  className={cn(
                    'outline-none text-md font-light h-[28px] overflow-visible border-transparent bg-transparent focus:ring-transparent focus:outline-none focus:border-transparent',
                    selectedValues[0]?.label === UnselectedOption ||
                      (selectedValues[0]?.label === undefined
                        ? 'placeholder:text-grey'
                        : 'placeholder:text-black'),
                    'w-full'
                  )}
                  placeholder={
                    selectedValues.length === 0 && !inputValue
                      ? buttonLabel || 'Select...'
                      : selectedValues[0]?.label
                  }
                />
              ) : (
                <span
                  className={cn(
                    'font-light select-none',
                    !isMulti
                      ? selectedValues[0]?.label === UnselectedOption
                        ? 'text-text-grey'
                        : 'text-text-black'
                      : 'text-text-grey'
                  )}
                >
                  {!isMulti
                    ? selectedValues[0]?.label || buttonLabel || 'Select...'
                    : selectedValues.length === 0
                      ? buttonLabel || 'Select...'
                      : ''}
                </span>
              )}

              {/* select arrow */}
              {!disabled && (
                <Icon
                  name={isSearchable ? 'magnifying-glass' : 'chevron-down'}
                  className={cn(
                    'absolute right-4 transition duration-300 ease-out',
                    open && !isSearchable && 'rotate-180'
                  )}
                />
              )}
            </div>
            {!disabled && (
              <Transition
                show={open}
                enter="transition duration-100 ease-out"
                enterFrom="opacity-0 scale-95 -translate-y-2"
                enterTo="opacity-100 scale-100 translate-y-0"
                leave="transition duration-100 ease-in"
                leaveFrom="opacity-100 scale-100 translate-y-0"
                leaveTo="opacity-0 scale-95 -translate-y-2"
              >
                <div
                  className={cn(
                    'bg-white-50 border !border-white-950 rounded-lg shadow-lg absolute max-h-[320px] overflow-y-auto z-50 mt-2',
                    size === 'full' && 'min-w-full'
                  )}
                >
                  {/* Renders multi or single options */}
                  {filteredOptions.map((option) => {
                    const isSelected = selectedValues.find((o) => o.value === option.value);

                    return isMulti ? (
                      <div
                        key={option.value}
                        onClick={(e) => {
                          if (option.disabled) return;
                          e.stopPropagation();
                          if (isSearchable) {
                            inputRef.current!.value = '';
                            setInputValue('');
                          }
                          handleSelect(option);
                        }}
                        className={cn(
                          DropdownSize,
                          'flex items-center gap-2 h-[44px] p-2 hover:bg-white-200 transition duration-300 cursor-pointer',
                          option.disabled ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white-200'
                        )}
                      >
                        {option.icon && <Icon name={option.icon} />}
                        <span className="select-none">{highlightMatch(option.label, query)}</span>
                        {isSelected && <Icon name="check" className="ml-auto text-green-500" />}
                      </div>
                    ) : (
                      <ListboxOption
                        disabled={option.disabled || false}
                        key={option.value}
                        value={option}
                        onClick={() => {
                          handleSelect(option);
                          if (isSearchable) {
                            inputRef.current!.value = '';
                            setInputValue('');
                          }
                          isOpen(false);
                        }}
                        className={cn(
                          'flex items-center gap-2 h-[44px] p-2 transition duration-300',
                          option.disabled ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white-200',
                          DropdownSize
                        )}
                      >
                        {option.icon && <Icon name={option.icon} />}
                        <span className="select-none font-light">
                          {highlightMatch(option.label, query)}
                        </span>
                      </ListboxOption>
                    );
                  })}
                </div>
              </Transition>
            )}
          </Listbox>
        </Field>
      </div>
    );
  }
);

Select.displayName = 'Select';
