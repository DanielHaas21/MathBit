'use client';
import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { TableVirtuoso, TableVirtuosoHandle } from 'react-virtuoso';

type BaseTableAttributes = Pick<
  React.TableHTMLAttributes<HTMLTableElement>,
  'id' | 'className' | 'style' | 'role' | 'onScroll'
>;

const tableVariants = cva('w-full text-left', {
  variants: {
    size: {
      xs: '',
      sm: '',
      md: '',
      lg: '',
      xl: ''
    },
    variant: {
      default: '',
      striped: ''
    },
    rounded: {
      true: ''
    }
  },
  defaultVariants: {
    variant: 'default',
    size: 'lg',
    rounded: true
  }
});

type TableVariantProps = VariantProps<typeof tableVariants>;
export interface TableProps<T = any> extends TableVariantProps, BaseTableAttributes {
  headers: React.ReactNode[];
  data: T[];
  renderRow?: (row: T, index: number) => React.ReactNode[];
  footer?: React.ReactNode[];
  className?: string;
  sticky?: boolean;
  NoContentFallback?: React.ReactNode;
  rowRef?: React.Ref<React.TdHTMLAttributes<HTMLTableColElement>>;
  selectedRows?: number[];
  rowClassName?: string;
  cellClassName?: string;
}

const arrayRowRenderer = (row: any[], index: number): React.ReactNode[] => {
  return row.map((r, rx) => <span key={`table-${index}-${rx}`}>{r}</span>);
};

export const Table = React.forwardRef<TableVirtuosoHandle, TableProps>(
  (
    {
      headers,
      data,
      renderRow,
      size,
      sticky = true,
      variant,
      rounded = true,
      footer,
      className,
      NoContentFallback,
      selectedRows,
      onScroll,
      cellClassName,
      rowClassName,
      ...props
    },
    ref
  ) => {
    const SizeValue = {
      xs: '300px',
      sm: '400px',
      md: '500px',
      lg: '600px',
      xl: '800px'
    }[size ?? 'md'];
    const rowRenderer = renderRow || arrayRowRenderer;

    return (
      <div
        className={cn('border border-white-500 overflow-hidden', rounded && 'rounded-[12px]')}
        style={{ height: SizeValue }}
      >
        <TableVirtuoso
          ref={ref}
          onScroll={onScroll}
          data={data}
          style={{
            height: SizeValue,
            overflow: 'auto'
          }}
          className="table-scroll-container min-w-full"
          fixedHeaderContent={() => (
            <tr>
              {headers.map((header, index) => (
                <th
                  key={index}
                  className={cn(
                    'border-b border-white-500 p-2 px-3 text-black-500 font-normal',
                    variant === 'default' ? 'bg-white-100' : 'bg-white-50'
                  )}
                >
                  {header}
                </th>
              ))}
            </tr>
          )}
          fixedFooterContent={() => (
            <tr className={cn(variant === 'default' ? 'bg-white-100' : 'bg-white-50')}>
              {footer &&
                footer.map((cell, index) => (
                  <td
                    key={index}
                    className="p-2 px-3 font-medium text-black-500 border-t border-white-500"
                  >
                    {cell}
                  </td>
                ))}
            </tr>
          )}
          components={{
            Table: ({ children, ...tableProps }) => (
              <table
                {...tableProps}
                className={cn(
                  tableVariants({ size, rounded, variant, className }),
                  'table-fixed overflow-auto',
                  !sticky && 'overflow-hidden'
                )}
                {...props}
              >
                {children}
              </table>
            ),
            TableRow: ({ children, ...props }) => {
              const index = props['data-index']; // From Virtuoso internal index

              const isSelected = selectedRows?.includes(index);

              return (
                <tr
                  {...props}
                  className={cn(
                    'transition ease-out duration-600',
                    variant === 'striped'
                      ? 'odd:bg-white-100 even:hover:bg-white-100 odd:hover:bg-white-50'
                      : 'bg-white-50',
                    isSelected && 'bg-white-300 transition ease-out duration-600', // Highlight selected
                    rowClassName
                  )}
                >
                  {children}
                </tr>
              );
            },
            EmptyPlaceholder: () => (
              <tbody>
                <tr>
                  <td colSpan={headers.length} className="p-0">
                    <div
                      style={{ height: `calc(${SizeValue} - 50px)` }}
                      className="flex justify-center items-center text-gray-500"
                    >
                      {NoContentFallback ?? <p>empty state</p>}
                    </div>
                  </td>
                </tr>
              </tbody>
            )
          }}
          itemContent={(index, row) => {
            const cells = rowRenderer(row, index);
            return (
              <React.Fragment key={index}>
                {cells.map((cell, cellIndex) => (
                  <td
                    key={cellIndex}
                    className={cn('border-b text-black-500 border-b-white-500 p-4', cellClassName)}
                  >
                    {cell}
                  </td>
                ))}
              </React.Fragment>
            );
          }}
        />
      </div>
    );
  }
);

Table.displayName = 'Table';
